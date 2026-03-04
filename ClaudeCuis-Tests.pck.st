'From Cuis 6.0 [latest update: #6411] on 18 January 2026 at 12:00:00 pm'!
'Description ClaudeCuis Test Suite - Unit tests for ClaudeHandler'!

!provides: 'ClaudeCuis-Tests' 1 0!
!requires: 'ClaudeCuis' 1 0 nil!

SystemOrganization addCategory: #'ClaudeCuis-Tests'!


!classDefinition: #MockMqttClient category: #'ClaudeCuis-Tests'!
Object subclass: #MockMqttClient
	instanceVariableNames: 'subscriptions publishedMessages'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClaudeCuis-Tests'!

!MockMqttClient commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
MockMqttClient is a test double for MQTTClientInterface.
It captures published messages and allows simulating incoming messages for testing.!


!classDefinition: #ClaudeHandlerTest category: #'ClaudeCuis-Tests'!
TestCase subclass: #ClaudeHandlerTest
	instanceVariableNames: 'handler mockMqtt testClassName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ClaudeCuis-Tests'!

!ClaudeHandlerTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
ClaudeHandlerTest provides unit tests for the ClaudeHandler class.
Run tests with: ClaudeHandlerTest suite run!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MockMqttClient methods"!

!MockMqttClient methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	super initialize.
	subscriptions := Dictionary new.
	publishedMessages := OrderedCollection new! !

!MockMqttClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
subscriptions
	^ subscriptions! !

!MockMqttClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
publishedMessages
	^ publishedMessages! !

!MockMqttClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
lastPublishedMessage
	"Return the last published message as a dictionary with topic, message, qos"
	publishedMessages isEmpty ifTrue: [^ nil].
	^ publishedMessages last! !

!MockMqttClient methodsFor: 'accessing' stamp: 'JMM 1/19/2026 12:00'!
lastPublishedJson
	"Return the last published message parsed as JSON. UTF-8 decode first."
	| msg |
	msg := self lastPublishedMessage.
	msg ifNil: [^ nil].
	^ Json readFrom: (UTF8Encoder decode: (msg at: 'message')) readStream! !

!MockMqttClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
subscribeTo: topicString qos: qos do: aBlock
	"Record subscription and store the handler block"
	subscriptions at: topicString put: aBlock! !

!MockMqttClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
unsubscribeFrom: aTopic
	"Remove subscription"
	subscriptions removeKey: aTopic ifAbsent: []! !

!MockMqttClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
publish: aByteArray onTopic: topicString qos: qos
	"Record published message"
	| msg |
	msg := Dictionary new.
	msg at: 'topic' put: topicString.
	msg at: 'message' put: aByteArray.
	msg at: 'qos' put: qos.
	publishedMessages add: msg! !

!MockMqttClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
disconnect
	"No-op for testing"! !

!MockMqttClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
username: uName password: pwd
	"No-op for testing"! !

!MockMqttClient methodsFor: 'testing' stamp: 'JMM 1/19/2026 12:00'!
simulateMessage: jsonString onTopic: topic
	"Simulate receiving a message - invoke the subscription handler. UTF-8 encode."
	| block |
	block := subscriptions at: topic ifAbsent: [^ self].
	block value: topic value: (UTF8Encoder encode: jsonString) value: 1! !

!MockMqttClient methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
clearPublishedMessages
	"Clear the published messages list"
	publishedMessages := OrderedCollection new! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"ClaudeHandlerTest methods - setup"!

!ClaudeHandlerTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
setUp
	super setUp.
	mockMqtt := MockMqttClient new.
	handler := ClaudeHandler on: mockMqtt imageId: 'test1'.
	handler start.
	testClassName := 'ClaudeTestClass'! !

!ClaudeHandlerTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
tearDown
	"Clean up any test classes we created"
	(Smalltalk includesKey: testClassName asSymbol) ifTrue: [
		(Smalltalk at: testClassName asSymbol) removeFromSystem].
	super tearDown! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"ClaudeHandlerTest methods - helper"!

!ClaudeHandlerTest methodsFor: 'helper' stamp: 'JMM 1/18/2026 12:00'!
sendRequest: action payload: payload
	"Helper to send a request and get the parsed response"
	| request requestId |
	requestId := 'test-', Time primMillisecondClock printString.
	request := Dictionary new.
	request at: 'requestId' put: requestId.
	request at: 'action' put: action.
	request at: 'payload' put: payload.

	mockMqtt clearPublishedMessages.
	mockMqtt simulateMessage: (Json render: request) onTopic: 'claude/request/test1'.

	^ mockMqtt lastPublishedJson! !

!ClaudeHandlerTest methodsFor: 'helper' stamp: 'JMM 1/18/2026 12:00'!
createTestClass
	"Create a temporary test class for testing"
	Compiler evaluate: 'Object subclass: #', testClassName, '
		instanceVariableNames: ''testVar''
		classVariableNames: ''TestClassVar''
		poolDictionaries: ''''
		category: ''ClaudeCuis-Tests'''! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"ClaudeHandlerTest methods - tests"!

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEvaluateSimple
	"Test basic arithmetic evaluation"
	| response |
	response := self sendRequest: 'evaluate' payload: (Dictionary new at: 'code' put: '3 + 4'; yourself).

	self assert: (response at: 'result') equals: '7'.
	self assert: (response at: 'imageId') equals: 'test1'.
	self deny: (response includesKey: 'error')! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEvaluateString
	"Test string evaluation"
	| response |
	response := self sendRequest: 'evaluate' payload: (Dictionary new at: 'code' put: '''hello'' size'; yourself).

	self assert: (response at: 'result') equals: '5'! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEvaluateWithError
	"Test that errors are handled properly"
	| response |
	response := self sendRequest: 'evaluate' payload: (Dictionary new at: 'code' put: '1 / 0'; yourself).

	self assert: (response includesKey: 'error')! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testBrowseClass
	"Test browsing class metadata"
	| response result |
	response := self sendRequest: 'browse' payload: (Dictionary new at: 'className' put: 'Object'; yourself).

	result := Json readFrom: (response at: 'result') readStream.
	self assert: (result at: 'name') equals: 'Object'.
	self assert: (result at: 'superclass') equals: 'ProtoObject'.
	self assert: (result includesKey: 'methods')! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testBrowseClassNotFound
	"Test browsing a non-existent class"
	| response |
	response := self sendRequest: 'browse' payload: (Dictionary new at: 'className' put: 'NonExistentClass12345'; yourself).

	self assert: ((response at: 'result') includesSubString: 'Error')! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testMethodSource
	"Test getting method source"
	| response |
	response := self sendRequest: 'methodSource' payload: (Dictionary new at: 'className' put: 'Object'; at: 'selector' put: 'class'; yourself).

	self assert: ((response at: 'result') includesSubString: 'class')! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testDefineAndDeleteMethod
	"Test defining and then deleting a method"
	| response |
	self createTestClass.

	"Define a method"
	response := self sendRequest: 'defineMethod' payload: (Dictionary new
		at: 'className' put: testClassName;
		at: 'source' put: 'testMethod ^ 42';
		at: 'category' put: 'testing';
		yourself).

	self assert: ((response at: 'result') includesSubString: 'successfully').

	"Verify method exists"
	self assert: ((Smalltalk at: testClassName asSymbol) includesSelector: #testMethod).

	"Delete the method"
	response := self sendRequest: 'deleteMethod' payload: (Dictionary new
		at: 'className' put: testClassName;
		at: 'selector' put: 'testMethod';
		yourself).

	self assert: ((response at: 'result') includesSubString: 'successfully').
	self deny: ((Smalltalk at: testClassName asSymbol) includesSelector: #testMethod)! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testDefineAndDeleteClass
	"Test defining and deleting a class"
	| response newClassName |
	newClassName := 'ClaudeTempTestClass'.

	"Make sure it doesn't exist"
	(Smalltalk includesKey: newClassName asSymbol) ifTrue: [
		(Smalltalk at: newClassName asSymbol) removeFromSystem].

	"Define a class"
	response := self sendRequest: 'defineClass' payload: (Dictionary new
		at: 'definition' put: 'Object subclass: #', newClassName, '
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: ''ClaudeCuis-Tests''';
		yourself).

	self assert: ((response at: 'result') includesSubString: 'successfully').
	self assert: (Smalltalk includesKey: newClassName asSymbol).

	"Delete the class"
	response := self sendRequest: 'deleteClass' payload: (Dictionary new at: 'className' put: newClassName; yourself).

	self assert: ((response at: 'result') includesSubString: 'successfully').
	self deny: (Smalltalk includesKey: newClassName asSymbol)! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testListClasses
	"Test listing classes"
	| response result |
	response := self sendRequest: 'listClasses' payload: (Dictionary new at: 'prefix' put: 'Array'; yourself).

	result := Json readFrom: (response at: 'result') readStream.
	self assert: (result includes: 'Array')! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testHierarchy
	"Test getting class hierarchy"
	| response result |
	response := self sendRequest: 'hierarchy' payload: (Dictionary new at: 'className' put: 'Integer'; yourself).

	result := Json readFrom: (response at: 'result') readStream.
	self assert: result first equals: 'Integer'.
	self assert: (result includes: 'Object')! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testSubclasses
	"Test getting subclasses"
	| response result |
	response := self sendRequest: 'subclasses' payload: (Dictionary new at: 'className' put: 'Collection'; yourself).

	result := Json readFrom: (response at: 'result') readStream.
	self assert: result isArray.
	self assert: result notEmpty! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testListCategories
	"Test listing categories"
	| response result |
	response := self sendRequest: 'listCategories' payload: Dictionary new.

	result := Json readFrom: (response at: 'result') readStream.
	self assert: result isArray.
	self assert: result notEmpty! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testClassesInCategory
	"Test getting classes in a category"
	| response result |
	response := self sendRequest: 'classesInCategory' payload: (Dictionary new at: 'category' put: 'Kernel-Objects'; yourself).

	result := Json readFrom: (response at: 'result') readStream.
	self assert: result isArray! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testDispatchUnknownAction
	"Test that unknown actions are handled"
	| response |
	response := self sendRequest: 'unknownAction' payload: Dictionary new.

	self assert: ((response at: 'result') includesSubString: 'Unknown action')! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testMalformedJson
	"Test handling of malformed JSON"
	| requestId |
	requestId := 'test-malformed'.
	mockMqtt clearPublishedMessages.
	mockMqtt simulateMessage: 'this is not valid json {{{' onTopic: 'claude/request/test1'.

	"Should have published an error response"
	self assert: mockMqtt publishedMessages notEmpty.
	self assert: (mockMqtt lastPublishedJson includesKey: 'error')! !

!ClaudeHandlerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEmptyPayload
	"Test that empty payload is handled"
	| response |
	response := self sendRequest: 'evaluate' payload: Dictionary new.

	self assert: ((response at: 'result') includesSubString: 'Error')! !

