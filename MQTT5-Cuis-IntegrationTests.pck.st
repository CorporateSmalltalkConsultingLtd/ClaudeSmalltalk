'From Cuis7.7 [latest update: #7777] on 21 January 2026 at 5:21:21 pm'!
'Description MQTT5-Cuis Integration Tests - Tests against a real MQTT v5.0 broker'!
!provides: 'MQTT5-Cuis-IntegrationTests' 1 1!
!requires: 'MQTT5-Cuis' 1 0 nil!
SystemOrganization addCategory: #'MQTT5-Cuis-IntegrationTests'!


!classDefinition: #MQTT5IntegrationTest category: #'MQTT5-Cuis-IntegrationTests'!
TestCase subclass: #MQTT5IntegrationTest
	instanceVariableNames: 'client'
	classVariableNames: 'BrokerHost BrokerPassword BrokerPort BrokerUsername'
	poolDictionaries: ''
	category: 'MQTT5-Cuis-IntegrationTests'!
!classDefinition: 'MQTT5IntegrationTest class' category: #'MQTT5-Cuis-IntegrationTests'!
MQTT5IntegrationTest class
	instanceVariableNames: ''!

!classDefinition: #MQTT5ConnectionTest category: #'MQTT5-Cuis-IntegrationTests'!
MQTT5IntegrationTest subclass: #MQTT5ConnectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Cuis-IntegrationTests'!
!classDefinition: 'MQTT5ConnectionTest class' category: #'MQTT5-Cuis-IntegrationTests'!
MQTT5ConnectionTest class
	instanceVariableNames: ''!

!classDefinition: #MQTT5PropertiesIntegrationTest category: #'MQTT5-Cuis-IntegrationTests'!
MQTT5IntegrationTest subclass: #MQTT5PropertiesIntegrationTest
	instanceVariableNames: 'receivedProperties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Cuis-IntegrationTests'!
!classDefinition: 'MQTT5PropertiesIntegrationTest class' category: #'MQTT5-Cuis-IntegrationTests'!
MQTT5PropertiesIntegrationTest class
	instanceVariableNames: ''!

!classDefinition: #MQTT5PublishSubscribeTest category: #'MQTT5-Cuis-IntegrationTests'!
MQTT5IntegrationTest subclass: #MQTT5PublishSubscribeTest
	instanceVariableNames: 'received receivedTopic'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Cuis-IntegrationTests'!
!classDefinition: 'MQTT5PublishSubscribeTest class' category: #'MQTT5-Cuis-IntegrationTests'!
MQTT5PublishSubscribeTest class
	instanceVariableNames: ''!


!MQTT5IntegrationTest methodsFor: 'running' stamp: ''!
setUp
	super setUp.
	client := nil.! !

!MQTT5IntegrationTest methodsFor: 'running' stamp: ''!
tearDown
	client ifNotNil: [
		[ client disconnect ]
			on: Error
			do: [ :argm0_1 |
				nil ]].
	client := nil.
	super tearDown.! !

!MQTT5IntegrationTest methodsFor: 'helpers' stamp: ''!
connectClient
	^ self connectClientWithID: self uniqueClientID.! !

!MQTT5IntegrationTest methodsFor: 'helpers' stamp: ''!
connectClientWithID: arg1
	client := MQTTClientInterface
		openOnHostName: self class brokerHost
		port: self class brokerPort
		keepAlive: 60.
	client useVersion50.
	client clientID: arg1.
	client
		username: self class brokerUsername
		password: self class brokerPassword.
	client connect.
	^ client.! !

!MQTT5IntegrationTest methodsFor: 'helpers' stamp: ''!
uniqueClientID
	^ 'CuisV5-' , Time primMillisecondClock printString , '-' , 100000 atRandom printString.! !

!MQTT5IntegrationTest methodsFor: 'helpers' stamp: ''!
uniqueTopic
	^ 'cuis/test/v5/' , Time primMillisecondClock printString , '/' , 100000 atRandom printString.! !

!MQTT5IntegrationTest class methodsFor: 'configuration' stamp: ''!
brokerHost
	^ BrokerHost ifNil: [ '192.168.1.158' ].! !

!MQTT5IntegrationTest class methodsFor: 'configuration' stamp: ''!
brokerPassword
	^ BrokerPassword ifNil: [ 'manager' ].! !

!MQTT5IntegrationTest class methodsFor: 'configuration' stamp: ''!
brokerPort
	^ BrokerPort ifNil: [ 1883 ].! !

!MQTT5IntegrationTest class methodsFor: 'configuration' stamp: ''!
brokerUsername
	^ BrokerUsername ifNil: [ 'Hudson' ].! !

!MQTT5IntegrationTest class methodsFor: 'configuration' stamp: ''!
configureBroker: arg1 port: arg2 username: arg3 password: arg4
	BrokerHost := arg1.
	BrokerPort := arg2.
	BrokerUsername := arg3.
	BrokerPassword := arg4.! !

!MQTT5IntegrationTest class methodsFor: 'testing' stamp: ''!
isAbstract
	^ self == MQTT5IntegrationTest.! !

!MQTT5ConnectionTest methodsFor: 'tests' stamp: ''!
testCleanStart
	| temp1 |
	temp1 := self uniqueClientID.
	self connectClientWithID: temp1.
	self assert: client isConnected.
	client disconnect.
	self connectClientWithID: temp1.
	self assert: client isConnected.! !

!MQTT5ConnectionTest methodsFor: 'tests' stamp: ''!
testConnectDisconnect
	self connectClient.
	self assert: client isConnected.
	client disconnect.
	self deny: client isConnected.! !

!MQTT5ConnectionTest methodsFor: 'tests' stamp: ''!
testConnectWithVersion5
	self connectClient.
	self assert: client isConnected.
	self assert: client isVersion5.! !

!MQTT5ConnectionTest methodsFor: 'tests' stamp: ''!
testServerCapabilities
	| temp1 |
	self connectClient.
	temp1 := client socketClient transport serverCapabilities.
	self assert: temp1 notNil.
	self assert: temp1 topicAliasMaximum isInteger.! !

!MQTT5PropertiesIntegrationTest methodsFor: 'running' stamp: ''!
setUp
	super setUp.
	receivedProperties := nil.! !

!MQTT5PropertiesIntegrationTest methodsFor: 'tests' stamp: ''!
testMessageExpiry
	| temp1 |
	temp1 := self uniqueTopic.
	self connectClient.
	self assert: client isVersion5.! !

!MQTT5PropertiesIntegrationTest methodsFor: 'tests' stamp: ''!
testResponseTopic
	| temp1 temp2 |
	temp1 := self uniqueTopic.
	temp2 := self uniqueTopic.
	self connectClient.
	self assert: client isVersion5.! !

!MQTT5PropertiesIntegrationTest methodsFor: 'tests' stamp: ''!
testUserProperties
	self connectClient.
	self assert: client isVersion5.
	self assert: client isConnected.! !

!MQTT5PublishSubscribeTest methodsFor: 'running' stamp: ''!
setUp
	super setUp.
	received := nil.
	receivedTopic := nil.! !

!MQTT5PublishSubscribeTest methodsFor: 'tests' stamp: ''!
testPublishSubscribeQoS0
	| temp1 temp2 |
	temp1 := self uniqueTopic.
	temp2 := 'Hello MQTT5 QoS0'.
	self connectClient.
	client
		subscribeTo: temp1
		qos: 0
		do: [ :argm3_3 :argm3_4 :argm3_5 |
			receivedTopic := argm3_3.
			received := UTF8Encoder decode: argm3_4 ].
	(Delay forMilliseconds: 200) wait.
	client
		publish: (UTF8Encoder encode: temp2)
		onTopic: temp1
		qos: 0.
	(Delay forMilliseconds: 500) wait.
	self
		assert: received
		equals: temp2.
	self
		assert: receivedTopic
		equals: temp1.! !

!MQTT5PublishSubscribeTest methodsFor: 'tests' stamp: ''!
testPublishSubscribeQoS1
	| temp1 temp2 |
	temp1 := self uniqueTopic.
	temp2 := 'Hello MQTT5 QoS1'.
	self connectClient.
	client
		subscribeTo: temp1
		qos: 1
		do: [ :argm3_3 :argm3_4 :argm3_5 |
			receivedTopic := argm3_3.
			received := UTF8Encoder decode: argm3_4 ].
	(Delay forMilliseconds: 200) wait.
	client
		publish: (UTF8Encoder encode: temp2)
		onTopic: temp1
		qos: 1.
	(Delay forMilliseconds: 500) wait.
	self
		assert: received
		equals: temp2.! !

!MQTT5PublishSubscribeTest methodsFor: 'tests' stamp: ''!
testPublishSubscribeUTF8
	| temp1 temp2 temp3 |
	temp1 := self uniqueTopic.
	temp3 := UTF8Encoder decode: #[227 131 134 227 130 185 227 131 136].
	temp2 := 'Test ' , temp3.
	self connectClient.
	client
		subscribeTo: temp1
		qos: 1
		do: [ :argm4_4 :argm4_5 :argm4_6 |
			received := UTF8Encoder decode: argm4_5 ].
	(Delay forMilliseconds: 200) wait.
	client
		publish: (UTF8Encoder encode: temp2)
		onTopic: temp1
		qos: 1.
	(Delay forMilliseconds: 500) wait.
	self
		assert: received
		equals: temp2.! !

!MQTT5PublishSubscribeTest methodsFor: 'tests' stamp: ''!
testUnsubscribe
	| temp1 temp2 temp3 temp4 |
	temp1 := self uniqueTopic.
	temp2 := 'Before unsub'.
	temp3 := nil.
	temp4 := nil.
	self connectClient.
	client
		subscribeTo: temp1
		qos: 1
		do: [ :argm5_5 :argm5_6 :argm5_7 |
			received := UTF8Encoder decode: argm5_6 ].
	(Delay forMilliseconds: 200) wait.
	client
		publish: (UTF8Encoder encode: temp2)
		onTopic: temp1
		qos: 1.
	(Delay forMilliseconds: 500) wait.
	temp3 := received.
	client unsubscribeFrom: temp1.
	(Delay forMilliseconds: 200) wait.
	received := nil.
	client
		publish: (UTF8Encoder encode: 'After unsub')
		onTopic: temp1
		qos: 1.
	(Delay forMilliseconds: 500) wait.
	temp4 := received.
	self
		assert: temp3
		equals: temp2.
	self assert: temp4 isNil.! !

!MQTT5PublishSubscribeTest methodsFor: 'tests' stamp: ''!
testWildcardSubscription
	| temp1 temp2 temp3 |
	temp1 := 'cuis/v5test/' , Time primMillisecondClock printString.
	temp2 := temp1 , '/sensor1'.
	temp3 := 'Wildcard test'.
	self connectClient.
	client
		subscribeTo: temp1 , '/#'
		qos: 1
		do: [ :argm4_4 :argm4_5 :argm4_6 |
			receivedTopic := argm4_4.
			received := UTF8Encoder decode: argm4_5 ].
	(Delay forMilliseconds: 200) wait.
	client
		publish: (UTF8Encoder encode: temp3)
		onTopic: temp2
		qos: 1.
	(Delay forMilliseconds: 500) wait.
	self
		assert: received
		equals: temp3.
	self
		assert: receivedTopic
		equals: temp2.! !
