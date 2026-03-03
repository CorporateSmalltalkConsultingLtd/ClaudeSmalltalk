Object subclass: #SeagullLLMHandler
  instanceVariableNames: 
    ' mqtt imageId running '
  classVariableNames: ''
  poolDictionaries: '' !


!SeagullLLMHandler class methods !

brokerRecordNamed: brokerName imageId: anImageId delegateEvents: delegate 

	"Factory method. Opens M0659 MQTT connection via broker record and returns a handler."

    | mqttInterface |
    mqttInterface := MQTTPahoClientInterfaceTopicPrefix
        openAndStartBrokerRecordNamed: brokerName
        clean: true
        clientId: nil
        delegateEvents: delegate.
    ^self new
        mqtt: mqttInterface;
        imageId: anImageId;
        yourself!

comment

"SeagullLLMHandler provides an MQTT-based interface for LLM agents (OpenClaw, Claude, etc.)
to interact with a live Seagull Smalltalk image via the M0659 MQTT API.

External dependencies:
  - MQTTPahoClientInterfaceTopicPrefix (M0659 MQTT v5 client)
  - JsonObjectEncoder class>>encode:
  - JsonObjectDecoder class>>decode:
  - CompilerInterface class>>evaluate:
  - CompilerInterface class>>compile:in:notifying:ifFail:
  - Behavior>>sourceCodeAt:
  - Object>>allClasses

Request format (JSON on topic 'claude/request/{imageId}'):
{
  'requestId': 'uuid-string',
  'action': 'evaluate',
  'payload': { 'code': '3 + 4' }
}

Response format (JSON on topic 'claude/response/{requestId}'):
{
  'requestId': 'uuid-string',
  'imageId': 'seagull1',
  'result': '7'
}

Usage:
  | handler |
  handler := SeagullLLMHandler
    brokerRecordNamed: 'MyBroker'
    imageId: 'seagull1'
    delegateEvents: nil.
  handler start.
  handler stop."

    !

on: mqttInterface imageId: anImageId 

	"Create a handler on an already-connected M0659 MQTT interface."

    ^self new
        mqtt: mqttInterface;
        imageId: anImageId;
        yourself! !


!SeagullLLMHandler methods !

browse: payload 

	"Return class metadata. Seagull has no class category or comment accessors."

    | className class info methods instVars classVars |
    className := payload at: 'className' ifAbsent: [''].
    className isEmpty ifTrue: [^'Error: No className provided'].
    class := Smalltalk at: className asSymbol ifAbsent: [^'Error: Class not found: ', className].
    methods := class selectors asSortedCollection asArray.
    instVars := class instVarNames.
    classVars := class classVarNames asArray.
    info := Dictionary new.
    info at: 'name' put: class name.
    info at: 'superclass' put: (class superclass ifNil: ['nil'] ifNotNil: [:s | s name]).
    info at: 'instanceVariables' put: instVars asArray.
    info at: 'classVariables' put: classVars.
    info at: 'methods' put: methods.
    ^info!

defineClass: payload 

	"Compile class definition via CompilerInterface."

    | definition |
    definition := payload at: 'definition' ifAbsent: [''].
    definition isEmpty ifTrue: [^'Error: No definition provided'].
    CompilerInterface evaluate: definition.
    ^'Class defined successfully'!

defineMethod: payload 

	"Compile a method using CompilerInterface, store source, and install via addSelector:withMethod:.
	 CompilerInterface compile:in:notifying:ifFail: compiles but does NOT install the method.
	 We must manually set sourceObject: (for source browsing) and addSelector:withMethod: (to install)."

    | className source class assoc |
    className := payload at: 'className' ifAbsent: [''].
    source := payload at: 'source' ifAbsent: [''].
    className isEmpty ifTrue: [^'Error: No className provided'].
    source isEmpty ifTrue: [^'Error: No source provided'].
    class := Smalltalk at: className asSymbol ifAbsent: [^'Error: Class not found: ', className].
    assoc := CompilerInterface compile: source in: class notifying: nil ifFail: [^'Error: Compilation failed for ', className].
    assoc value sourceObject: source.
    class addSelector: assoc key withMethod: assoc value.
    ^'Method defined successfully'!

deleteClass: payload 

	"Remove a class from the system."

    | className class |
    className := payload at: 'className' ifAbsent: [''].
    className isEmpty ifTrue: [^'Error: No className provided'].
    class := Smalltalk at: className asSymbol ifAbsent: [^'Error: Class not found: ', className].
    class removeFromSystem.
    ^'Class deleted successfully'!

deleteMethod: payload 

	"Remove a method from a class."

    | className selector class |
    className := payload at: 'className' ifAbsent: [''].
    selector := payload at: 'selector' ifAbsent: [''].
    className isEmpty ifTrue: [^'Error: No className provided'].
    selector isEmpty ifTrue: [^'Error: No selector provided'].
    class := Smalltalk at: className asSymbol ifAbsent: [^'Error: Class not found: ', className].
    (class includesSelector: selector asSymbol) ifFalse: [
        ^'Error: Method not found: ', className, '>>', selector].
    class removeSelector: selector asSymbol.
    ^'Method deleted successfully'!

dispatch: action payload: payload 

	"Route action to appropriate handler method."

    action = 'evaluate' ifTrue: [^self evaluate: payload].
    action = 'browse' ifTrue: [^self browse: payload].
    action = 'methodSource' ifTrue: [^self methodSource: payload].
    action = 'defineClass' ifTrue: [^self defineClass: payload].
    action = 'defineMethod' ifTrue: [^self defineMethod: payload].
    action = 'deleteMethod' ifTrue: [^self deleteMethod: payload].
    action = 'deleteClass' ifTrue: [^self deleteClass: payload].
    action = 'listClasses' ifTrue: [^self listClasses: payload].
    action = 'hierarchy' ifTrue: [^self hierarchy: payload].
    action = 'subclasses' ifTrue: [^self subclasses: payload].
    action = 'saveImage' ifTrue: [^self saveImage: payload].
    ^'Unknown action: ', action!

evaluate: payload 

	"Execute code via CompilerInterface evaluate:"

    | code result |
    code := payload at: 'code' ifAbsent: [''].
    code isEmpty ifTrue: [^'Error: No code provided'].
    result := CompilerInterface evaluate: code.
    ^result printString!

handleMessage: jsonString 

	"Parse JSON, dispatch to action handler, send response.
	 M0659 subscribeToString already decoded UTF8 for us.
	 M0659 publishString will encode UTF8 for us."

    | request requestId action payload result response responseTopic responseJson |
    requestId := 'unknown'.
    [request := JsonObjectDecoder decode: jsonString.
    requestId := request at: 'requestId' ifAbsent: ['unknown'].
    action := request at: 'action' ifAbsent: [''].
    payload := request at: 'payload' ifAbsent: [Dictionary new].
    result := self dispatch: action payload: payload.
    response := Dictionary new.
    response at: 'requestId' put: requestId.
    response at: 'imageId' put: imageId.
    response at: 'result' put: result.
    responseTopic := 'claude/response/', requestId.
    responseJson := JsonObjectEncoder encode: response.
    mqtt
        publishBinary: responseJson
        onTopic: responseTopic
        qos: 1
        retain: false
        waitUntilAcknowledged: false.
    ] on: Error do: [:ex |
        | msg callStack stackTrace |
        msg := '' asStream.
        callStack := Process copyStack.
        callStack walkbackOn: msg maxLevels: 99.
        stackTrace := msg contents.
        self sendErrorResponse: requestId error: ex description stack: stackTrace]!

hierarchy: payload 

	"Walk superclass chain."

    | className class chain |
    className := payload at: 'className' ifAbsent: [''].
    className isEmpty ifTrue: [^'Error: No className provided'].
    class := Smalltalk at: className asSymbol ifAbsent: [^'Error: Class not found: ', className].
    chain := OrderedCollection new.
    [class notNil] whileTrue: [
        chain add: class name.
        class := class superclass].
    ^chain asArray!

imageId

	"Answer my 'imageId' instance variable."

    ^imageId!

imageId: anObject 

	"Set my 'imageId' instance variable to anObject."

    imageId := anObject!

listClasses: payload 

	"Filter all classes by prefix."

    | prefix classes result |
    prefix := payload at: 'prefix' ifAbsent: [''].
    classes := Object allClasses.
    prefix isEmpty ifFalse: [
        classes := classes select: [:c | c name asString beginsWith: prefix]].
    result := (classes collect: [:c | c name]) asSortedCollection asArray.
    ^result!

methodSource: payload 

	"Get method source via Behavior>>sourceCodeAt:"

    | className selector class |
    className := payload at: 'className' ifAbsent: [''].
    selector := payload at: 'selector' ifAbsent: [''].
    className isEmpty ifTrue: [^'Error: No className provided'].
    selector isEmpty ifTrue: [^'Error: No selector provided'].
    class := Smalltalk at: className asSymbol ifAbsent: [^'Error: Class not found: ', className].
    (class includesSelector: selector asSymbol) ifFalse: [
        ^'Error: Method not found: ', className, '>>', selector].
    ^class sourceCodeAt: selector asSymbol!

mqtt

	"Answer my 'mqtt' instance variable."

    ^mqtt!

mqtt: anObject 

	"Set my 'mqtt' instance variable to anObject."

    mqtt := anObject!

printOn: aStream 

	"print useful data"

    super printOn: aStream.
    aStream nextPutAll: ' imageId: '.
    imageId printOn: aStream.
    aStream nextPutAll: ' running: '.
    self running printOn: aStream.!

running

	"Answer my 'running' instance variable."

    ^running ifNil: [running := false]!

saveImage: payload 

	"Save the Smalltalk image."

    Smalltalk saveImage.
    ^'Image saved successfully'!

sendErrorResponse: requestId error: errorMessage stack: stackTrace 

	"Send an error response via MQTT."

    | response responseTopic responseJson |
    response := Dictionary new.
    response at: 'requestId' put: requestId.
    response at: 'imageId' put: imageId.
    response at: 'error' put: errorMessage.
    response at: 'stack' put: stackTrace.
    responseTopic := 'claude/response/', requestId.
    responseJson := JsonObjectEncoder encode: response.
    mqtt
        publishBinary: responseJson
        onTopic: responseTopic
        qos: 1
        retain: false
        waitUntilAcknowledged: false!

start

	"Subscribe to request topic and begin processing.
	 M0659 subscribeToString handles UTF8 decoding of incoming messages."

    | topic |
    topic := 'claude/request/', imageId.
    mqtt
        subscribeToString: topic
        qos: 1
        waitUntilAcknowledged: false
        do: [:t :msg :q |
            self handleMessage: msg].
    running := true.
    Transcript show: 'SeagullLLMHandler started on topic: ', topic; cr.
    ^self!

stop

	"Unsubscribe and disconnect."

    | topic |
    topic := 'claude/request/', imageId.
    mqtt unsubscribeFrom: topic waitUntilAcknowledged: false.
    mqtt stop.
    running := false.
    Transcript show: 'SeagullLLMHandler stopped'; cr.
    ^self!

subclasses: payload 

	"Return direct subclasses of a class."

    | className class subs |
    className := payload at: 'className' ifAbsent: [''].
    className isEmpty ifTrue: [^'Error: No className provided'].
    class := Smalltalk at: className asSymbol ifAbsent: [^'Error: Class not found: ', className].
    subs := (class subclasses collect: [:c | c name]) asSortedCollection asArray.
    ^subs! !
