SystemOrganization addCategory: #'MQTT5-Squeak-IntegrationTests'!

TestCase subclass: #MQTT5IntegrationTest
	instanceVariableNames: 'client'
	classVariableNames: 'BrokerHost BrokerPassword BrokerPort BrokerUsername'
	poolDictionaries: ''
	category: 'MQTT5-Squeak-IntegrationTests'!

MQTT5IntegrationTest subclass: #MQTT5ConnectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-IntegrationTests'!

MQTT5IntegrationTest subclass: #MQTT5PropertiesIntegrationTest
	instanceVariableNames: 'receivedProperties receivedMessage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-IntegrationTests'!

MQTT5IntegrationTest subclass: #MQTT5PublishSubscribeTest
	instanceVariableNames: 'received receivedTopic'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-IntegrationTests'!


!MQTT5IntegrationTest methodsFor: 'running' stamp: ''!
setUp
	super setUp.
	client := nil.! !

!MQTT5IntegrationTest methodsFor: 'running' stamp: ''!
tearDown
	client ifNotNil: [
		[ client disconnect ]
			on: Error
			do: [ :argm0_1 | nil ] ].
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
	^ 'SqV5-' , Time millisecondClockValue printString , '-' , 100000 atRandom printString.! !

!MQTT5IntegrationTest methodsFor: 'helpers' stamp: ''!
uniqueTopic
	^ 'squeak/test/v5/' , Time millisecondClockValue printString , '/' , 100000 atRandom printString.! !

!MQTT5IntegrationTest class methodsFor: 'configuration' stamp: ''!
brokerHost
	^ BrokerHost ifNil: [ '192.168.6.6' ].! !

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
	receivedProperties := nil.
	receivedMessage := nil.! !

!MQTT5PropertiesIntegrationTest methodsFor: 'tests' stamp: ''!
testMessageExpiry
	| topic props message |
	topic := self uniqueTopic.
	message := 'Message with expiry'.
	self connectClient.
	self assert: client isVersion5.
	client
		subscribeTo: topic
		qos: 1
		do: [ :t :p :pr |
			receivedMessage := UTF8Encoder decode: p.
			receivedProperties := pr ].
	(Delay forMilliseconds: 200) wait.
	props := MQTT5Properties new.
	props addPropertyIdentifier: MQTT5Constants messageExpiryInterval value: 1.
	client
		publish: (UTF8Encoder encode: message)
		onTopic: topic
		qos: 1
		properties: props.
	(Delay forMilliseconds: 500) wait.
	self assert: receivedMessage equals: message.
	self assert: (receivedProperties propertyAt: MQTT5Constants messageExpiryInterval) notNil.! !

!MQTT5PropertiesIntegrationTest methodsFor: 'tests' stamp: ''!
testResponseTopic
	| topic responseTopic props message |
	topic := self uniqueTopic.
	responseTopic := self uniqueTopic.
	message := 'Message with response topic'.
	self connectClient.
	self assert: client isVersion5.
	client
		subscribeTo: topic
		qos: 1
		do: [ :t :p :pr |
			receivedMessage := UTF8Encoder decode: p.
			receivedProperties := pr ].
	(Delay forMilliseconds: 200) wait.
	props := MQTT5Properties new.
	props addPropertyIdentifier: MQTT5Constants responseTopic value: responseTopic.
	client
		publish: (UTF8Encoder encode: message)
		onTopic: topic
		qos: 1
		properties: props.
	(Delay forMilliseconds: 500) wait.
	self assert: receivedMessage equals: message.
	self assert: (receivedProperties propertyAt: MQTT5Constants responseTopic) equals: responseTopic.! !

!MQTT5PropertiesIntegrationTest methodsFor: 'tests' stamp: ''!
testUserProperties
	| topic props message values |
	topic := self uniqueTopic.
	message := 'Message with user properties'.
	self connectClient.
	self assert: client isVersion5.
	self assert: client isConnected.
	client
		subscribeTo: topic
		qos: 1
		do: [ :t :p :pr |
			receivedMessage := UTF8Encoder decode: p.
			receivedProperties := pr ].
	(Delay forMilliseconds: 200) wait.
	props := MQTT5Properties new.
	props addPropertyIdentifier: 16r26 value: #('key1' 'value1').
	props addPropertyIdentifier: 16r26 value: #('key2' 'value2').
	client
		publish: (UTF8Encoder encode: message)
		onTopic: topic
		qos: 1
		properties: props.
	(Delay forMilliseconds: 500) wait.
	values := receivedProperties propertiesAt: 16r26.
	self assert: receivedMessage equals: message.
	self assert: values size equals: 2.
	self assert: (values first first) equals: 'key1'.
	self assert: (values second second) equals: 'value2'.! !


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
	self assert: received equals: temp2.
	self assert: receivedTopic equals: temp1.! !

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
	self assert: received equals: temp2.! !

!MQTT5PublishSubscribeTest methodsFor: 'tests' stamp: ''!
testPublishSubscribeQoS2
	| topic message |
	topic := self uniqueTopic.
	message := 'Hello MQTT5 QoS2'.
	self connectClient.
	client
		subscribeTo: topic
		qos: 2
		do: [ :t :p :pr |
			receivedTopic := t.
			received := UTF8Encoder decode: p ].
	(Delay forMilliseconds: 200) wait.
	client
		publish: (UTF8Encoder encode: message)
		onTopic: topic
		qos: 2.
	(Delay forMilliseconds: 800) wait.
	self assert: received equals: message.
	self assert: receivedTopic equals: topic.! !

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
	self assert: received equals: temp2.! !

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
	self assert: temp3 equals: temp2.
	self assert: temp4 isNil.! !

!MQTT5PublishSubscribeTest methodsFor: 'tests' stamp: ''!
testWildcardSubscription
	| temp1 temp2 temp3 |
	temp1 := 'squeak/v5test/' , Time millisecondClockValue printString.
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
	self assert: received equals: temp3.
	self assert: receivedTopic equals: temp2.! !

!MQTT5PublishSubscribeTest methodsFor: 'tests' stamp: ''!
testRetainedMessages
	| topic message |
	topic := self uniqueTopic.
	message := 'Retained message'.
	self connectClient.
	client
		publish: (UTF8Encoder encode: message)
		onTopic: topic
		qos: 1
		retain: true.
	(Delay forMilliseconds: 200) wait.
	received := nil.
	client
		subscribeTo: topic
		qos: 1
		do: [ :t :p :pr |
			received := UTF8Encoder decode: p ].
	(Delay forMilliseconds: 500) wait.
	self assert: received equals: message.! !

!MQTT5PublishSubscribeTest methodsFor: 'tests' stamp: ''!
testLargeMessage
	| topic message |
	topic := self uniqueTopic.
	message := String new: 200000 withAll: $A.
	self connectClient.
	client
		subscribeTo: topic
		qos: 1
		do: [ :t :p :pr |
			received := UTF8Encoder decode: p ].
	(Delay forMilliseconds: 200) wait.
	client
		publish: (UTF8Encoder encode: message)
		onTopic: topic
		qos: 1.
	(Delay forMilliseconds: 1000) wait.
	self assert: received size equals: message size.
	self assert: received equals: message.! !