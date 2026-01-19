'From Cuis 6.0 [latest update: #6411] on 18 January 2026 at 12:00:00 pm'!
'Description MQTT-Cuis Integration Tests - Tests against a real MQTT broker'!

!provides: 'MQTT-Cuis-IntegrationTests' 1 0!
!requires: 'MQTT-Cuis' 1 0 nil!

SystemOrganization addCategory: #'MQTT-Cuis-IntegrationTests'!


!classDefinition: #MQTTIntegrationTest category: #'MQTT-Cuis-IntegrationTests'!
TestCase subclass: #MQTTIntegrationTest
	instanceVariableNames: 'client'
	classVariableNames: 'BrokerHost BrokerPort BrokerUsername BrokerPassword'
	poolDictionaries: ''
	category: 'MQTT-Cuis-IntegrationTests'!

!MQTTIntegrationTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
MQTTIntegrationTest tests MQTT client against a real broker.

Configure broker settings before running:
  MQTTIntegrationTest configureBroker: 'your-broker' port: 1883 username: 'your-user' password: 'your-pass'.

Run tests:
  MQTTIntegrationTest buildSuite run inspect!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTIntegrationTest class methods"!

!MQTTIntegrationTest class methodsFor: 'configuration' stamp: 'JMM 1/18/2026 12:00'!
configureBroker: host port: port username: user password: pwd
	"Configure broker connection settings"
	BrokerHost := host.
	BrokerPort := port.
	BrokerUsername := user.
	BrokerPassword := pwd! !

!MQTTIntegrationTest class methodsFor: 'configuration' stamp: 'JMM 1/18/2026 12:00'!
brokerHost
	^ BrokerHost ifNil: ['localhost']! !

!MQTTIntegrationTest class methodsFor: 'configuration' stamp: 'JMM 1/18/2026 12:00'!
brokerPort
	^ BrokerPort ifNil: [1883]! !

!MQTTIntegrationTest class methodsFor: 'configuration' stamp: 'JMM 1/18/2026 12:00'!
brokerUsername
	^ BrokerUsername! !

!MQTTIntegrationTest class methodsFor: 'configuration' stamp: 'JMM 1/18/2026 12:00'!
brokerPassword
	^ BrokerPassword! !

!MQTTIntegrationTest class methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isAbstract
	"Skip this class, run subclass tests"
	^ self == MQTTIntegrationTest! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTIntegrationTest instance methods"!

!MQTTIntegrationTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
setUp
	super setUp.
	client := nil! !

!MQTTIntegrationTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
tearDown
	client ifNotNil: [
		[client disconnect] on: Error do: [:ex | "ignore disconnect errors"]].
	client := nil.
	super tearDown! !

!MQTTIntegrationTest methodsFor: 'helpers' stamp: 'JMM 1/18/2026 12:00'!
connectClient
	"Create and connect client to broker with unique client ID"
	^ self connectClientWithID: self uniqueClientID! !

!MQTTIntegrationTest methodsFor: 'helpers' stamp: 'JMM 1/18/2026 12:00'!
connectClientWithID: clientID
	"Create and connect client to broker with specified client ID"
	client := MQTTClientInterface
		openOnHostName: self class brokerHost
		port: self class brokerPort
		keepAlive: 60.
	client clientID: clientID.
	client username: self class brokerUsername password: self class brokerPassword.
	client connect.
	^ client! !

!MQTTIntegrationTest methodsFor: 'helpers' stamp: 'JMM 1/18/2026 12:00'!
uniqueClientID
	"Generate a unique client ID"
	^ 'CuisTest-', Time primMillisecondClock printString, '-', (100000 atRandom) printString! !

!MQTTIntegrationTest methodsFor: 'helpers' stamp: 'JMM 1/18/2026 12:00'!
uniqueTopic
	"Generate a unique topic for testing"
	^ 'cuis/test/', Time primMillisecondClock printString! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: #MQTTConnectionTest category: #'MQTT-Cuis-IntegrationTests'!
MQTTIntegrationTest subclass: #MQTTConnectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-IntegrationTests'!

!MQTTConnectionTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
Tests basic MQTT broker connection.!


!MQTTConnectionTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnect
	"Test basic connection to broker"
	self connectClient.
	self assert: client notNil.
	self assert: client isConnected! !

!MQTTConnectionTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnectDisconnect
	"Test connect and disconnect cycle"
	self connectClient.
	self assert: client isConnected.
	client disconnect.
	self deny: client isConnected! !

!MQTTConnectionTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testBadCredentials
	"Test connection with bad credentials results in disconnection.
	 MQTT CONNACK with bad credentials (return code 4 or 5) causes disconnect."
	client := MQTTClientInterface
		openOnHostName: self class brokerHost
		port: self class brokerPort
		keepAlive: 60.
	client clientID: self uniqueClientID.
	client username: 'baduser' password: 'badpass'.
	client connect.
	"Wait for CONNACK to arrive and be processed"
	(Delay forSeconds: 4) wait.
	"Client should no longer be connected after bad CONNACK"
	self deny: client isConnected description: 'Should disconnect after bad credentials'! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: #MQTTPublishSubscribeTest category: #'MQTT-Cuis-IntegrationTests'!
MQTTIntegrationTest subclass: #MQTTPublishSubscribeTest
	instanceVariableNames: 'receivedMessages'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-IntegrationTests'!

!MQTTPublishSubscribeTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
Tests MQTT publish and subscribe functionality.!


!MQTTPublishSubscribeTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
setUp
	super setUp.
	receivedMessages := OrderedCollection new! !

!MQTTPublishSubscribeTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testSubscribe
	"Test subscribing to a topic"
	| topic |
	self connectClient.
	topic := self uniqueTopic.
	client subscribeTo: topic qos: 1 do: [:t :msg :q |
		receivedMessages add: msg].
	"Give broker time to process subscription"
	(Delay forMilliseconds: 100) wait.
	self assert: client isConnected! !

!MQTTPublishSubscribeTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPublishQos0
	"Test publishing with QoS 0 (fire and forget)"
	| topic |
	self connectClient.
	topic := self uniqueTopic.
	client publish: 'hello' asByteArray onTopic: topic qos: 0.
	self assert: client isConnected! !

!MQTTPublishSubscribeTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPublishQos1
	"Test publishing with QoS 1 (at least once)"
	| topic |
	self connectClient.
	topic := self uniqueTopic.
	client publish: 'hello qos1' asByteArray onTopic: topic qos: 1.
	"Give broker time to send PUBACK"
	(Delay forMilliseconds: 100) wait.
	self assert: client isConnected! !

!MQTTPublishSubscribeTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPublishSubscribeRoundtrip
	"Test that published message is received by subscriber.
	 Uses two clients: one subscribes, another publishes."
	| topic testMessage received semaphore publisherClient |
	self connectClient.  "This client will subscribe"
	topic := self uniqueTopic.
	testMessage := 'test message ', Time primMillisecondClock printString.
	received := nil.
	semaphore := Semaphore new.

	"Subscribe first"
	client subscribeTo: topic qos: 1 do: [:t :msg :q |
		Transcript show: 'Received message on topic: ', t; cr.
		received := msg asString.
		semaphore signal].

	"Wait for subscription to be established"
	(Delay forMilliseconds: 300) wait.

	"Create second client to publish (with different client ID)"
	publisherClient := MQTTClientInterface
		openOnHostName: self class brokerHost
		port: self class brokerPort
		keepAlive: 60.
	publisherClient clientID: self uniqueClientID.
	publisherClient username: self class brokerUsername password: self class brokerPassword.
	publisherClient connect.
	(Delay forMilliseconds: 200) wait.

	[
		"Publish message from second client"
		Transcript show: 'Publishing to topic: ', topic; cr.
		publisherClient publish: testMessage asByteArray onTopic: topic qos: 1.

		"Wait for message with timeout"
		(semaphore waitTimeoutMSecs: 3000) ifTrue: [
			self failWith: 'Timeout waiting for message'].

		self assert: received equals: testMessage
	] ensure: [publisherClient disconnect]! !

!MQTTPublishSubscribeTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testUnsubscribe
	"Test unsubscribing from a topic"
	| topic |
	self connectClient.
	topic := self uniqueTopic.

	"Subscribe"
	client subscribeTo: topic qos: 1 do: [:t :msg :q | ].
	(Delay forMilliseconds: 100) wait.

	"Unsubscribe"
	client unsubscribeFrom: topic.
	(Delay forMilliseconds: 100) wait.

	self assert: client isConnected! !

!MQTTPublishSubscribeTest methodsFor: 'tests' stamp: 'JMM 1/19/2026 12:00'!
testPublishSubscribeUnicode
	"Test that Japanese/unicode characters round-trip correctly.
	 Uses two clients: one subscribes, another publishes.
	 Japanese string 'konnichiha' (hello) built from UTF-8 bytes."
	| topic testBytes testMessage received semaphore publisherClient |
	self connectClient.  "This client will subscribe"
	topic := self uniqueTopic.
	"UTF-8 bytes for Japanese 'konnichiha' (hello): ko n ni chi ha"
	testBytes := #[16rE3 16r81 16r93 16rE3 16r82 16r93 16rE3 16r81 16rAB 16rE3 16r81 16rA1 16rE3 16r81 16rAF].
	testMessage := UTF8Encoder decode: testBytes.
	received := nil.
	semaphore := Semaphore new.

	"Subscribe first"
	client subscribeTo: topic qos: 1 do: [:t :msg :q |
		received := UTF8Encoder decode: msg.
		semaphore signal].

	"Wait for subscription to be established"
	(Delay forMilliseconds: 300) wait.

	"Create second client to publish (with different client ID)"
	publisherClient := MQTTClientInterface
		openOnHostName: self class brokerHost
		port: self class brokerPort
		keepAlive: 60.
	publisherClient clientID: self uniqueClientID.
	publisherClient username: self class brokerUsername password: self class brokerPassword.
	publisherClient connect.
	(Delay forMilliseconds: 200) wait.

	[
		"Publish UTF-8 encoded message from second client"
		publisherClient publish: (UTF8Encoder encode: testMessage) onTopic: topic qos: 1.

		"Wait for message with timeout"
		(semaphore waitTimeoutMSecs: 3000) ifTrue: [
			self failWith: 'Timeout waiting for message'].

		self assert: received equals: testMessage
	] ensure: [publisherClient disconnect]! !

!MQTTPublishSubscribeTest methodsFor: 'tests' stamp: 'JMM 1/19/2026 12:00'!
testPublishSubscribeUnicodeTopic
	"Test that Japanese/unicode characters work in topic strings.
	 Uses two clients: one subscribes, another publishes.
	 Topic includes Japanese 'test' (tesuto)."
	| baseTopic japanesePart topic testMessage received semaphore publisherClient |
	self connectClient.  "This client will subscribe"
	baseTopic := self uniqueTopic.
	"UTF-8 bytes for Japanese 'tesuto' (test): te su to"
	japanesePart := UTF8Encoder decode: #[16rE3 16r83 16r86 16rE3 16r82 16rB9 16rE3 16r83 16r88].
	topic := baseTopic, '/', japanesePart.
	testMessage := 'hello'.
	received := nil.
	semaphore := Semaphore new.

	"Subscribe first"
	client subscribeTo: topic qos: 1 do: [:t :msg :q |
		received := msg asString.
		semaphore signal].

	"Wait for subscription to be established"
	(Delay forMilliseconds: 300) wait.

	"Create second client to publish (with different client ID)"
	publisherClient := MQTTClientInterface
		openOnHostName: self class brokerHost
		port: self class brokerPort
		keepAlive: 60.
	publisherClient clientID: self uniqueClientID.
	publisherClient username: self class brokerUsername password: self class brokerPassword.
	publisherClient connect.
	(Delay forMilliseconds: 200) wait.

	[
		"Publish message to unicode topic"
		publisherClient publish: testMessage asByteArray onTopic: topic qos: 1.

		"Wait for message with timeout"
		(semaphore waitTimeoutMSecs: 3000) ifTrue: [
			self failWith: 'Timeout waiting for message'].

		self assert: received equals: testMessage
	] ensure: [publisherClient disconnect]! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

!classDefinition: #MQTTClaudeIntegrationTest category: #'MQTT-Cuis-IntegrationTests'!
MQTTIntegrationTest subclass: #MQTTClaudeIntegrationTest
	instanceVariableNames: 'handler responseReceived responseSemaphore lastResponse'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-IntegrationTests'!

!MQTTClaudeIntegrationTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
Tests ClaudeHandler with real MQTT broker.
Requires ClaudeCuis package to be loaded.!


!MQTTClaudeIntegrationTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
setUp
	super setUp.
	responseReceived := false.
	responseSemaphore := Semaphore new.
	lastResponse := nil.
	handler := nil! !

!MQTTClaudeIntegrationTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
tearDown
	handler ifNotNil: [
		[handler stop] on: Error do: [:ex | ]].
	super tearDown! !

!MQTTClaudeIntegrationTest methodsFor: 'helpers' stamp: 'JMM 1/18/2026 12:00'!
startHandler
	"Start ClaudeHandler on test image ID"
	self connectClient.
	handler := (Smalltalk at: #ClaudeHandler ifAbsent: [^ self skip: 'ClaudeCuis not loaded'])
		on: client imageId: 'test-integration'.
	handler start.
	"Give handler time to subscribe"
	(Delay forMilliseconds: 200) wait! !

!MQTTClaudeIntegrationTest methodsFor: 'helpers' stamp: 'JMM 1/19/2026 12:00'!
sendRequest: action payload: payload
	"Send a request and wait for response. UTF-8 encode/decode for JSON."
	| requestId request responseTopic secondClient |
	requestId := 'test-', Time primMillisecondClock printString.

	"Create second client to send request and receive response (with unique client ID)"
	secondClient := MQTTClientInterface
		openOnHostName: self class brokerHost
		port: self class brokerPort
		keepAlive: 60.
	secondClient clientID: self uniqueClientID.
	secondClient username: self class brokerUsername password: self class brokerPassword.
	secondClient connect.

	[responseTopic := 'claude/response/', requestId.
	secondClient subscribeTo: responseTopic qos: 1 do: [:t :msg :q |
		lastResponse := Json readFrom: (UTF8Encoder decode: msg) readStream.
		responseSemaphore signal].

	"Wait for subscription"
	(Delay forMilliseconds: 100) wait.

	"Build and send request"
	request := Dictionary new.
	request at: 'requestId' put: requestId.
	request at: 'action' put: action.
	request at: 'payload' put: payload.

	secondClient publish: (UTF8Encoder encode: (Json render: request))
		onTopic: 'claude/request/test-integration' qos: 1.

	"Wait for response"
	(responseSemaphore waitTimeoutMSecs: 5000) ifTrue: [
		self failWith: 'Timeout waiting for response'].

	^ lastResponse]
		ensure: [secondClient disconnect]! !

!MQTTClaudeIntegrationTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testHandlerStart
	"Test that handler starts and subscribes"
	self startHandler.
	self assert: handler running! !

!MQTTClaudeIntegrationTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEvaluateSimple
	"Test evaluate action via real MQTT"
	| response |
	self startHandler.
	response := self sendRequest: 'evaluate' payload: (Dictionary new at: 'code' put: '3 + 4'; yourself).
	self assert: (response at: 'result') equals: '7'.
	self assert: (response at: 'imageId') equals: 'test-integration'! !

!MQTTClaudeIntegrationTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testBrowseClass
	"Test browse action via real MQTT"
	| response result |
	self startHandler.
	response := self sendRequest: 'browse' payload: (Dictionary new at: 'className' put: 'Object'; yourself).
	result := Json readFrom: (response at: 'result') readStream.
	self assert: (result at: 'name') equals: 'Object'! !

!MQTTClaudeIntegrationTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testListCategories
	"Test listCategories action via real MQTT"
	| response result |
	self startHandler.
	response := self sendRequest: 'listCategories' payload: Dictionary new.
	result := Json readFrom: (response at: 'result') readStream.
	self assert: result isArray.
	self assert: result notEmpty! !

!MQTTClaudeIntegrationTest methodsFor: 'tests' stamp: 'JMM 1/19/2026 12:00'!
testEvaluateUnicode
	"Test that Japanese/unicode characters work in evaluate action.
	 Japanese 'konnichiha' (hello) built from UTF-8 bytes."
	| response japaneseHello code |
	self startHandler.
	"UTF-8 bytes for Japanese 'konnichiha': ko n ni chi ha"
	japaneseHello := UTF8Encoder decode: #[16rE3 16r81 16r93 16rE3 16r82 16r93 16rE3 16r81 16rAB 16rE3 16r81 16rA1 16rE3 16r81 16rAF].
	code := '''', japaneseHello, ''' size'.
	response := self sendRequest: 'evaluate' payload: (Dictionary new at: 'code' put: code; yourself).
	self assert: (response at: 'result') equals: '5'.
	self deny: (response includesKey: 'error')! !

!MQTTClaudeIntegrationTest methodsFor: 'tests' stamp: 'JMM 1/19/2026 12:00'!
testEvaluateUnicodeRoundtrip
	"Test that Japanese characters round-trip through evaluate.
	 Japanese 'konnichiha' (hello) built from UTF-8 bytes."
	| response japaneseHello code expectedResult |
	self startHandler.
	"UTF-8 bytes for Japanese 'konnichiha': ko n ni chi ha"
	japaneseHello := UTF8Encoder decode: #[16rE3 16r81 16r93 16rE3 16r82 16r93 16rE3 16r81 16rAB 16rE3 16r81 16rA1 16rE3 16r81 16rAF].
	code := '''', japaneseHello, ''''.
	expectedResult := '''', japaneseHello, ''''.
	response := self sendRequest: 'evaluate' payload: (Dictionary new at: 'code' put: code; yourself).
	self assert: (response at: 'result') equals: expectedResult.
	self deny: (response includesKey: 'error')! !

