'From Cuis 6.0 [latest update: #6411] on 18 January 2026 at 12:00:00 pm'!
'Description ClaudeCuis - MQTT-based interface for Claude to interact with Cuis Smalltalk'!

!provides: 'ClaudeCuis' 1 0!
!requires: 'MQTT-Cuis' 1 0 nil!
!requires: 'JSON' 1 0 nil!

SystemOrganization addCategory: #ClaudeCuis!


!classDefinition: #ClaudeHandler category: #ClaudeCuis!
Object subclass: #ClaudeHandler
	instanceVariableNames: 'mqtt imageId running'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClaudeCuis'!

!ClaudeHandler commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
ClaudeHandler provides an MQTT-based interface for Claude to interact with a live Cuis Smalltalk image.

Request format (JSON on topic 'claude/request/{imageId}'):
{
  "requestId": "uuid-string",
  "action": "evaluate",
  "payload": { "code": "3 + 4" }
}

Response format (JSON on topic 'claude/response/{requestId}'):
{
  "requestId": "uuid-string",
  "imageId": "dev1",
  "result": "7"
}

Usage:
  | client handler |
  client := MQTTClientInterface openOnHostName: 'your-broker' port: 1883 keepAlive: 60.
  client username: 'your-user' password: 'your-pass'.
  client connect.
  handler := ClaudeHandler on: client imageId: 'dev1'.
  handler start.
!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"ClaudeHandler class methods"!

!ClaudeHandler class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
on: mqttClient imageId: anImageId
	"Factory method to create a handler"
	^ self new
		mqtt: mqttClient;
		imageId: anImageId;
		yourself! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"ClaudeHandler instance methods - accessing"!

!ClaudeHandler methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
imageId
	^ imageId! !

!ClaudeHandler methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
imageId: anObject
	imageId := anObject! !

!ClaudeHandler methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
mqtt
	^ mqtt! !

!ClaudeHandler methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
mqtt: anObject
	mqtt := anObject! !

!ClaudeHandler methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
running
	^ running ifNil: [running := false]! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"ClaudeHandler instance methods - control"!

!ClaudeHandler methodsFor: 'control' stamp: 'JMM 1/18/2026 12:00'!
start
	"Subscribe to request topic and begin processing"
	| topic |
	topic := 'claude/request/', imageId.
	mqtt subscribeTo: topic qos: 1 do: [:t :msg :q |
		self handleMessage: msg].
	running := true.
	Transcript show: 'ClaudeHandler started on topic: ', topic; cr.
	^ self! !

!ClaudeHandler methodsFor: 'control' stamp: 'JMM 1/18/2026 12:00'!
stop
	"Unsubscribe and disconnect"
	| topic |
	topic := 'claude/request/', imageId.
	mqtt unsubscribeFrom: topic.
	mqtt disconnect.
	running := false.
	Transcript show: 'ClaudeHandler stopped'; cr.
	^ self! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"ClaudeHandler instance methods - message handling"!

!ClaudeHandler methodsFor: 'message handling' stamp: 'JMM 1/19/2026 12:00'!
handleMessage: messageBytes
	"Parse JSON, dispatch to action handler, send response.
	 JSON is UTF-8 encoded, so decode on receive and encode on send."
	| jsonString request requestId action payload result response responseTopic |

	requestId := 'unknown'.
	[jsonString := UTF8Encoder decode: messageBytes.
	request := Json readFrom: jsonString readStream.

	requestId := request at: 'requestId' ifAbsent: ['unknown'].
	action := request at: 'action' ifAbsent: [''].
	payload := request at: 'payload' ifAbsent: [Dictionary new].

	result := self dispatch: action payload: payload.

	response := Dictionary new.
	response at: 'requestId' put: requestId.
	response at: 'imageId' put: imageId.
	response at: 'result' put: result.

	responseTopic := 'claude/response/', requestId.
	mqtt publish: (UTF8Encoder encode: (Json render: response)) onTopic: responseTopic qos: 1.

	] on: Error do: [:ex |
		self sendErrorResponse: requestId error: ex description stack: ex signalerContext shortStack]! !

!ClaudeHandler methodsFor: 'message handling' stamp: 'JMM 1/18/2026 12:00'!
dispatch: action payload: payload
	"Route action to appropriate handler method"
	action = 'evaluate' ifTrue: [^ self evaluate: payload].
	action = 'browse' ifTrue: [^ self browse: payload].
	action = 'methodSource' ifTrue: [^ self methodSource: payload].
	action = 'defineClass' ifTrue: [^ self defineClass: payload].
	action = 'defineMethod' ifTrue: [^ self defineMethod: payload].
	action = 'deleteMethod' ifTrue: [^ self deleteMethod: payload].
	action = 'deleteClass' ifTrue: [^ self deleteClass: payload].
	action = 'listClasses' ifTrue: [^ self listClasses: payload].
	action = 'hierarchy' ifTrue: [^ self hierarchy: payload].
	action = 'subclasses' ifTrue: [^ self subclasses: payload].
	action = 'listCategories' ifTrue: [^ self listCategories: payload].
	action = 'classesInCategory' ifTrue: [^ self classesInCategory: payload].
	action = 'saveImage' ifTrue: [^ self saveImage: payload].

	^ 'Unknown action: ', action! !

!ClaudeHandler methodsFor: 'message handling' stamp: 'JMM 1/19/2026 12:00'!
sendErrorResponse: requestId error: errorMessage stack: stackTrace
	"Send an error response. UTF-8 encode for MQTT."
	| response responseTopic |
	response := Dictionary new.
	response at: 'requestId' put: requestId.
	response at: 'imageId' put: imageId.
	response at: 'error' put: errorMessage.
	response at: 'stack' put: stackTrace.

	responseTopic := 'claude/response/', requestId.
	mqtt publish: (UTF8Encoder encode: (Json render: response)) onTopic: responseTopic qos: 1! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"ClaudeHandler instance methods - actions"!

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
evaluate: payload
	"Execute code via Compiler evaluate:"
	| code result |
	code := payload at: 'code' ifAbsent: [''].
	code isEmpty ifTrue: [^ 'Error: No code provided'].

	result := Compiler evaluate: code.
	^ result printString! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
browse: payload
	"Return class metadata"
	| className class info methods instVars classVars |
	className := payload at: 'className' ifAbsent: [''].
	className isEmpty ifTrue: [^ 'Error: No className provided'].

	class := Smalltalk at: className asSymbol ifAbsent: [^ 'Error: Class not found: ', className].

	methods := class selectors asArray sorted.
	instVars := class instVarNames.
	classVars := class classVarNames asArray.

	info := Dictionary new.
	info at: 'name' put: class name.
	info at: 'superclass' put: (class superclass ifNil: ['nil'] ifNotNil: [:s | s name]).
	info at: 'category' put: class category.
	info at: 'instanceVariables' put: instVars asArray.
	info at: 'classVariables' put: classVars.
	info at: 'methods' put: methods.
	info at: 'comment' put: (class comment ifNil: ['']).

	^ Json render: info! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
methodSource: payload
	"Get source via (class >> selector) sourceCode"
	| className selector class method |
	className := payload at: 'className' ifAbsent: [''].
	selector := payload at: 'selector' ifAbsent: [''].

	className isEmpty ifTrue: [^ 'Error: No className provided'].
	selector isEmpty ifTrue: [^ 'Error: No selector provided'].

	class := Smalltalk at: className asSymbol ifAbsent: [^ 'Error: Class not found: ', className].

	(class includesSelector: selector asSymbol) ifFalse: [
		^ 'Error: Method not found: ', className, '>>', selector].

	method := class >> selector asSymbol.
	^ method sourceCode! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
defineClass: payload
	"Compile class definition"
	| definition |
	definition := payload at: 'definition' ifAbsent: [''].
	definition isEmpty ifTrue: [^ 'Error: No definition provided'].

	Compiler evaluate: definition.
	^ 'Class defined successfully'! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
defineMethod: payload
	"Compile a method: class compile: source"
	| className source category class |
	className := payload at: 'className' ifAbsent: [''].
	source := payload at: 'source' ifAbsent: [''].
	category := payload at: 'category' ifAbsent: ['as yet unclassified'].

	className isEmpty ifTrue: [^ 'Error: No className provided'].
	source isEmpty ifTrue: [^ 'Error: No source provided'].

	class := Smalltalk at: className asSymbol ifAbsent: [^ 'Error: Class not found: ', className].

	class compile: source classified: category.
	^ 'Method defined successfully'! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
deleteMethod: payload
	"Remove a method: class removeSelector: selector"
	| className selector class |
	className := payload at: 'className' ifAbsent: [''].
	selector := payload at: 'selector' ifAbsent: [''].

	className isEmpty ifTrue: [^ 'Error: No className provided'].
	selector isEmpty ifTrue: [^ 'Error: No selector provided'].

	class := Smalltalk at: className asSymbol ifAbsent: [^ 'Error: Class not found: ', className].

	(class includesSelector: selector asSymbol) ifFalse: [
		^ 'Error: Method not found: ', className, '>>', selector].

	class removeSelector: selector asSymbol.
	^ 'Method deleted successfully'! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
deleteClass: payload
	"Remove a class: class removeFromSystem"
	| className class |
	className := payload at: 'className' ifAbsent: [''].
	className isEmpty ifTrue: [^ 'Error: No className provided'].

	class := Smalltalk at: className asSymbol ifAbsent: [^ 'Error: Class not found: ', className].

	class removeFromSystem.
	^ 'Class deleted successfully'! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
listClasses: payload
	"Filter Smalltalk allClasses by prefix"
	| prefix classes result |
	prefix := payload at: 'prefix' ifAbsent: [''].

	classes := Smalltalk allClasses.
	prefix isEmpty ifFalse: [
		classes := classes select: [:c | c name asString beginsWith: prefix]].

	result := (classes collect: [:c | c name]) asArray.
	result := result sorted.

	^ Json render: result! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
hierarchy: payload
	"Walk superclass chain"
	| className class chain |
	className := payload at: 'className' ifAbsent: [''].
	className isEmpty ifTrue: [^ 'Error: No className provided'].

	class := Smalltalk at: className asSymbol ifAbsent: [^ 'Error: Class not found: ', className].

	chain := OrderedCollection new.
	[class notNil] whileTrue: [
		chain add: class name.
		class := class superclass].

	^ Json render: chain asArray! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
subclasses: payload
	"Return class subclasses"
	| className class subs |
	className := payload at: 'className' ifAbsent: [''].
	className isEmpty ifTrue: [^ 'Error: No className provided'].

	class := Smalltalk at: className asSymbol ifAbsent: [^ 'Error: Class not found: ', className].

	subs := (class subclasses collect: [:c | c name]) asArray.
	subs := subs sorted.

	^ Json render: subs! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
listCategories: payload
	"Return SystemOrganization categories"
	| categories |
	categories := SystemOrganization categories asArray sorted.
	^ Json render: categories! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
classesInCategory: payload
	"Filter classes by category"
	| category classes result |
	category := payload at: 'category' ifAbsent: [''].
	category isEmpty ifTrue: [^ 'Error: No category provided'].

	classes := Smalltalk allClasses select: [:c | c category = category asSymbol].
	result := (classes collect: [:c | c name]) asArray.
	result := result sorted.

	^ Json render: result! !

!ClaudeHandler methodsFor: 'actions' stamp: 'JMM 1/18/2026 12:00'!
saveImage: payload
	"Save the Smalltalk image"
	Smalltalk saveImage.
	^ 'Image saved successfully'! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"ClaudeHandler instance methods - printing"!

!ClaudeHandler methodsFor: 'printing' stamp: 'JMM 1/18/2026 12:00'!
printOn: aStream
	super printOn: aStream.
	aStream
		nextPutAll: ' imageId: ';
		print: imageId;
		nextPutAll: ' running: ';
		print: self running! !

