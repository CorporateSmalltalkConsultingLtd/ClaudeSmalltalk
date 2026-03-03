'From Squeak 6.0 on 16 February 2026 at 2:58:00 am'!
'Description MQTT 5.0 Unit Tests for Squeak Smalltalk - Ported from Cuis'!

'Description MQTT 5.0 Test Suite for Cuis Smalltalk'!


SystemOrganization addCategory: #'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5PropertyTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5PropertiesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5ReasonCodeTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5ConstantsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5ServerCapabilitiesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5SubscriptionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5ConnectPacketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5ConnAckPacketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5PublishPacketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5SubscribePacketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5SubAckPacketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5UnsubscribePacketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5DisconnectPacketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5AuthPacketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!

TestCase subclass: #MQTT5PubAckPacketTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-Tests'!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5PropertyTest"!

!MQTT5PropertyTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testCreation
	| prop |
	prop := MQTT5Property identifier: 16r11 value: 3600.
	self assert: prop identifier equals: 16r11.
	self assert: prop value equals: 3600! !

!MQTT5PropertyTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testStringProperty
	| prop |
	prop := MQTT5Property identifier: 16r1F value: 'Test reason string'.
	self assert: prop identifier equals: 16r1F.
	self assert: prop value equals: 'Test reason string'! !

!MQTT5PropertyTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testBinaryProperty
	| prop data |
	data := #[1 2 3 4 5].
	prop := MQTT5Property identifier: 16r16 value: data.
	self assert: prop identifier equals: 16r16.
	self assert: prop value equals: data! !

!MQTT5PropertyTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testUserProperty
	| prop pair |
	pair := #('key' 'value').
	prop := MQTT5Property identifier: 16r26 value: pair.
	self assert: prop identifier equals: 16r26.
	self assert: prop value first equals: 'key'.
	self assert: prop value second equals: 'value'! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5PropertiesTest"!

!MQTT5PropertiesTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testInitialize
	| props |
	props := MQTT5Properties new.
	self assert: props isEmpty.
	self assert: props size equals: 0! !

!MQTT5PropertiesTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAddProperty
	| props |
	props := MQTT5Properties new.
	props addProperty: (MQTT5Property identifier: 16r11 value: 3600).
	self deny: props isEmpty.
	self assert: props size equals: 1! !

!MQTT5PropertiesTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPropertyAt
	| props |
	props := MQTT5Properties new.
	props addProperty: (MQTT5Property identifier: 16r11 value: 3600).
	props addProperty: (MQTT5Property identifier: 16r1F value: 'Reason').
	self assert: (props propertyAt: 16r11) equals: 3600.
	self assert: (props propertyAt: 16r1F) equals: 'Reason'.
	self assert: (props propertyAt: 16r99) isNil! !

!MQTT5PropertiesTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPropertiesAtMultiple
	| props values |
	props := MQTT5Properties new.
	props addProperty: (MQTT5Property identifier: 16r26 value: #('key1' 'val1')).
	props addProperty: (MQTT5Property identifier: 16r26 value: #('key2' 'val2')).
	values := props propertiesAt: 16r26.
	self assert: values size equals: 2! !

!MQTT5PropertiesTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAddPropertyIdentifierValue
	| props |
	props := MQTT5Properties new.
	props addPropertyIdentifier: 16r11 value: 7200.
	self assert: (props propertyAt: 16r11) equals: 7200! !

!MQTT5PropertiesTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testNotEmpty
	| props |
	props := MQTT5Properties new.
	self deny: props notEmpty.
	props addProperty: (MQTT5Property identifier: 16r11 value: 100).
	self assert: props notEmpty! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5ReasonCodeTest"!

!MQTT5ReasonCodeTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSuccessDescription
	self assert: (MQTT5ReasonCode descriptionFor: 16r00) equals: 'Success'! !

!MQTT5ReasonCodeTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testGrantedQoSDescriptions
	self assert: (MQTT5ReasonCode descriptionFor: 16r01) equals: 'Granted QoS 1'.
	self assert: (MQTT5ReasonCode descriptionFor: 16r02) equals: 'Granted QoS 2'! !

!MQTT5ReasonCodeTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testErrorDescriptions
	self assert: (MQTT5ReasonCode descriptionFor: 16r80) equals: 'Unspecified error'.
	self assert: (MQTT5ReasonCode descriptionFor: 16r87) equals: 'Not authorized'.
	self assert: (MQTT5ReasonCode descriptionFor: 16r8B) equals: 'Server shutting down'! !

!MQTT5ReasonCodeTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAuthReasonCodes
	self assert: (MQTT5ReasonCode descriptionFor: 16r18) equals: 'Continue authentication'.
	self assert: (MQTT5ReasonCode descriptionFor: 16r19) equals: 'Re-authenticate'! !

!MQTT5ReasonCodeTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testIsSuccess
	self assert: (MQTT5ReasonCode isSuccess: 16r00).
	self assert: (MQTT5ReasonCode isSuccess: 16r01).
	self assert: (MQTT5ReasonCode isSuccess: 16r02).
	self deny: (MQTT5ReasonCode isSuccess: 16r80)! !

!MQTT5ReasonCodeTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testIsError
	self assert: (MQTT5ReasonCode isError: 16r80).
	self assert: (MQTT5ReasonCode isError: 16r87).
	self deny: (MQTT5ReasonCode isError: 16r00).
	self deny: (MQTT5ReasonCode isError: 16r02)! !

!MQTT5ReasonCodeTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testUnknownReasonCode
	| desc |
	desc := MQTT5ReasonCode descriptionFor: 16rFF.
	self assert: (desc includesSubstring: 'Unknown')! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5ConstantsTest"!

!MQTT5ConstantsTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPropertyIdentifiers
	self assert: MQTT5Constants payloadFormatIndicator equals: 16r01.
	self assert: MQTT5Constants messageExpiryInterval equals: 16r02.
	self assert: MQTT5Constants contentType equals: 16r03.
	self assert: MQTT5Constants responseTopic equals: 16r08.
	self assert: MQTT5Constants correlationData equals: 16r09.
	self assert: MQTT5Constants subscriptionIdentifier equals: 16r0B.
	self assert: MQTT5Constants sessionExpiryInterval equals: 16r11.
	self assert: MQTT5Constants topicAlias equals: 16r23! !

!MQTT5ConstantsTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReasonCodes
	self assert: MQTT5Constants reasonCodeSuccess equals: 16r00.
	self assert: MQTT5Constants reasonCodeUnspecifiedError equals: 16r80.
	self assert: MQTT5Constants reasonCodeNotAuthorized equals: 16r87.
	self assert: MQTT5Constants reasonCodeTopicNameInvalid equals: 16r90! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5ServerCapabilitiesTest"!

!MQTT5ServerCapabilitiesTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testDefaultValues
	| caps |
	caps := MQTT5ServerCapabilities new.
	self assert: caps receiveMaximum equals: 65535.
	self assert: caps maximumQoS equals: 2.
	self assert: caps retainAvailable.
	self assert: caps topicAliasMaximum equals: 0.
	self assert: caps wildcardSubscriptionAvailable.
	self assert: caps subscriptionIdentifiersAvailable.
	self assert: caps sharedSubscriptionAvailable! !

!MQTT5ServerCapabilitiesTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSetters
	| caps |
	caps := MQTT5ServerCapabilities new.
	caps receiveMaximum: 100.
	caps maximumQoS: 1.
	caps retainAvailable: false.
	caps topicAliasMaximum: 10.
	self assert: caps receiveMaximum equals: 100.
	self assert: caps maximumQoS equals: 1.
	self deny: caps retainAvailable.
	self assert: caps topicAliasMaximum equals: 10! !

!MQTT5ServerCapabilitiesTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testUpdateFromConnAckProperties
	| caps props |
	caps := MQTT5ServerCapabilities new.
	props := MQTT5Properties new.
	props addProperty: (MQTT5Property identifier: 16r21 value: 50).  "Receive Maximum"
	props addProperty: (MQTT5Property identifier: 16r24 value: 1).   "Maximum QoS"
	props addProperty: (MQTT5Property identifier: 16r22 value: 20).  "Topic Alias Maximum"
	caps updateFromConnAckProperties: props.
	self assert: caps receiveMaximum equals: 50.
	self assert: caps maximumQoS equals: 1.
	self assert: caps topicAliasMaximum equals: 20! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5SubscriptionTest"!

!MQTT5SubscriptionTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testDefaultValues
	| sub |
	sub := MQTTSubscription new.
	self assert: sub qos equals: 0.
	self deny: sub noLocal.
	self deny: sub retainAsPublished.
	self assert: sub retainHandling equals: 0! !

!MQTT5SubscriptionTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSetters
	| sub |
	sub := MQTTSubscription new.
	sub topic: 'test/topic'.
	sub qos: 2.
	sub noLocal: true.
	sub retainAsPublished: true.
	sub retainHandling: 2.
	self assert: sub topic equals: 'test/topic'.
	self assert: sub qos equals: 2.
	self assert: sub noLocal.
	self assert: sub retainAsPublished.
	self assert: sub retainHandling equals: 2! !

!MQTT5SubscriptionTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSubscriptionOptionsV5QoSOnly
	| sub |
	sub := MQTTSubscription new.
	sub qos: 1.
	self assert: sub subscriptionOptionsV5 equals: 1! !

!MQTT5SubscriptionTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSubscriptionOptionsV5NoLocal
	| sub |
	sub := MQTTSubscription new.
	sub qos: 0.
	sub noLocal: true.
	self assert: sub subscriptionOptionsV5 equals: 4  "bit 2"! !

!MQTT5SubscriptionTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSubscriptionOptionsV5RetainAsPublished
	| sub |
	sub := MQTTSubscription new.
	sub qos: 0.
	sub retainAsPublished: true.
	self assert: sub subscriptionOptionsV5 equals: 8  "bit 3"! !

!MQTT5SubscriptionTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSubscriptionOptionsV5RetainHandling
	| sub |
	sub := MQTTSubscription new.
	sub qos: 0.
	sub retainHandling: 2.
	self assert: sub subscriptionOptionsV5 equals: 32  "bits 4-5 = 10 binary"! !

!MQTT5SubscriptionTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSubscriptionOptionsV5Combined
	| sub |
	sub := MQTTSubscription new.
	sub qos: 1.
	sub noLocal: true.
	sub retainAsPublished: true.
	sub retainHandling: 2.
	"QoS 1 (1) + No Local (4) + RAP (8) + RH 2 (32) = 45"
	self assert: sub subscriptionOptionsV5 equals: 45! !

!MQTT5SubscriptionTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSubscriptionOptionsV311
	| sub |
	sub := MQTTSubscription new.
	sub qos: 2.
	sub noLocal: true.  "ignored in v3.1.1"
	sub retainAsPublished: true.  "ignored in v3.1.1"
	self assert: sub subscriptionOptionsV311 equals: 2  "only QoS"! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5ConnectPacketTest"!

!MQTT5ConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPacketType
	| packet |
	packet := MQTTPacketConnect new.
	self assert: packet packetType equals: 1! !

!MQTT5ConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testDefaultProtocolVersion
	| packet |
	packet := MQTTPacketConnect new.
	self assert: packet protocolVersion equals: 5! !

!MQTT5ConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testClientID
	| packet |
	packet := MQTTPacketConnect new.
	packet clientID: 'TestClient123'.
	self assert: packet clientIDString equals: 'TestClient123'! !

!MQTT5ConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testCredentials
	| packet |
	packet := MQTTPacketConnect new.
	packet user: 'testUser'.
	packet password: 'testPass'.
	self assert: packet user equals: 'testUser'.
	self assert: packet password equals: 'testPass'! !

!MQTT5ConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSessionExpiryInterval
	| packet |
	packet := MQTTPacketConnect new.
	packet sessionExpiryInterval: 3600.
	self assert: packet sessionExpiryInterval equals: 3600! !

!MQTT5ConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReceiveMaximum
	| packet |
	packet := MQTTPacketConnect new.
	packet receiveMaximum: 100.
	self assert: packet receiveMaximum equals: 100! !

!MQTT5ConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testTopicAliasMaximum
	| packet |
	packet := MQTTPacketConnect new.
	packet topicAliasMaximum: 50.
	self assert: packet topicAliasMaximum equals: 50! !

!MQTT5ConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAuthenticationMethod
	| packet |
	packet := MQTTPacketConnect new.
	packet authenticationMethod: 'SCRAM-SHA-256'.
	self assert: packet authenticationMethod equals: 'SCRAM-SHA-256'! !

!MQTT5ConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testKeepAliveTime
	| packet |
	packet := MQTTPacketConnect new.
	packet keepAliveTime: 120.
	self assert: packet keepAliveTime equals: 120! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5ConnAckPacketTest"!

!MQTT5ConnAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPacketType
	| packet |
	packet := MQTTPacketConnAck new.
	self assert: packet packetType equals: 2! !

!MQTT5ConnAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReasonCode
	| packet |
	packet := MQTTPacketConnAck new.
	packet reasonCode: 16r87.
	self assert: packet reasonCode equals: 16r87! !

!MQTT5ConnAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSessionPresent
	| packet |
	packet := MQTTPacketConnAck new.
	packet sessionPresent: true.
	self assert: packet sessionPresent! !

!MQTT5ConnAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReasonCodeSuccess
	| packet |
	packet := MQTTPacketConnAck new.
	packet reasonCode: 0.
	self assert: (MQTT5ReasonCode isSuccess: packet reasonCode).
	packet reasonCode: 16r87.
	self assert: (MQTT5ReasonCode isError: packet reasonCode)! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5PublishPacketTest"!

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPacketType
	| packet |
	packet := MQTTPacketPublish new.
	self assert: packet packetType equals: 3! !

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testTopicAndMessage
	| packet |
	packet := MQTTPacketPublish new.
	packet topic: 'test/topic'.
	packet message: 'Hello World'.
	self assert: packet topic equals: 'test/topic'.
	self assert: packet message equals: 'Hello World'! !

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testTopicAlias
	| packet |
	packet := MQTTPacketPublish new.
	packet topicAlias: 42.
	self assert: packet topicAlias equals: 42! !

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testMessageExpiryInterval
	| packet |
	packet := MQTTPacketPublish new.
	packet messageExpiryInterval: 3600.
	self assert: packet messageExpiryInterval equals: 3600! !

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPayloadFormatIndicator
	| packet |
	packet := MQTTPacketPublish new.
	packet payloadFormatIndicator: 1.
	self assert: packet payloadFormatIndicator equals: 1! !

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testResponseTopic
	| packet |
	packet := MQTTPacketPublish new.
	packet responseTopic: 'reply/topic'.
	self assert: packet responseTopic equals: 'reply/topic'! !

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testCorrelationData
	| packet data |
	data := #[1 2 3 4].
	packet := MQTTPacketPublish new.
	packet correlationData: data.
	self assert: packet correlationData equals: data! !

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testContentType
	| packet |
	packet := MQTTPacketPublish new.
	packet contentType: 'application/json'.
	self assert: packet contentType equals: 'application/json'! !

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testQoSLevels
	| packet |
	packet := MQTTPacketPublish new.
	packet qos: 0.
	self assert: packet qos equals: 0.
	packet qos: 1.
	self assert: packet qos equals: 1.
	packet qos: 2.
	self assert: packet qos equals: 2! !

!MQTT5PublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testRetainFlag
	| packet |
	packet := MQTTPacketPublish new.
	packet retainFlag: true.
	"Retain flag is encoded in fixed header flags"
	self assert: (packet fixedHeaderFlags bitAnd: 1) equals: 1! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5SubscribePacketTest"!

!MQTT5SubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPacketType
	| packet |
	packet := MQTTPacketSubscribe new.
	self assert: packet packetType equals: 8! !

!MQTT5SubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testFixedHeaderFlags
	| packet |
	packet := MQTTPacketSubscribe new.
	self assert: packet fixedHeaderFlags equals: 2! !

!MQTT5SubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAddTopicSimple
	| packet |
	packet := MQTTPacketSubscribe new.
	packet addTopic: 'test/topic'.
	self assert: packet topics size equals: 1.
	self assert: (packet topics includes: 'test/topic')! !

!MQTT5SubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAddTopicWithQoS
	| packet |
	packet := MQTTPacketSubscribe new.
	packet addTopic: 'test/topic' qos: 2.
	self assert: packet topics size equals: 1! !

!MQTT5SubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAddTopicWithV5Options
	| packet |
	packet := MQTTPacketSubscribe new.
	packet addTopic: 'test/topic' qos: 1 noLocal: true retainAsPublished: true retainHandling: 2.
	self assert: packet topics size equals: 1! !

!MQTT5SubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSubscriptionIdentifier
	| packet |
	packet := MQTTPacketSubscribe new.
	packet subscriptionIdentifier: 12345.
	self assert: packet subscriptionIdentifier equals: 12345! !

!MQTT5SubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testMultipleTopics
	| packet |
	packet := MQTTPacketSubscribe new.
	packet addTopic: 'topic1' qos: 0.
	packet addTopic: 'topic2' qos: 1.
	packet addTopic: 'topic3' qos: 2.
	self assert: packet topics size equals: 3! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5SubAckPacketTest"!

!MQTT5SubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPacketType
	| packet |
	packet := MQTTPacketSubAck new.
	self assert: packet packetType equals: 9! !

!MQTT5SubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReasonCodes
	| packet |
	packet := MQTTPacketSubAck new.
	packet reasonCodes: #(0 1 2).
	self assert: packet reasonCodes size equals: 3.
	self assert: packet reasonCodes first equals: 0! !

!MQTT5SubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testIsSuccess
	| packet |
	packet := MQTTPacketSubAck new.
	packet reasonCodes: #(0 1 2).
	self assert: packet isSuccess.
	packet reasonCodes: #(0 16r80).
	self deny: packet isSuccess! !

!MQTT5SubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testHasFailures
	| packet |
	packet := MQTTPacketSubAck new.
	packet reasonCodes: #(0 1 2).
	self deny: packet hasFailures.
	packet reasonCodes: #(0 16r87).
	self assert: packet hasFailures! !

!MQTT5SubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReturnCodesAlias
	| packet |
	packet := MQTTPacketSubAck new.
	packet returnCodes: #(0 1).
	self assert: packet returnCodes equals: packet reasonCodes! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5UnsubscribePacketTest"!

!MQTT5UnsubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPacketType
	| packet |
	packet := MQTTPacketUnsubscribe new.
	self assert: packet packetType equals: 10! !

!MQTT5UnsubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testFixedHeaderFlags
	| packet |
	packet := MQTTPacketUnsubscribe new.
	self assert: packet fixedHeaderFlags equals: 2! !

!MQTT5UnsubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAddTopic
	| packet |
	packet := MQTTPacketUnsubscribe new.
	packet addTopic: 'test/topic'.
	self assert: packet topics size equals: 1.
	self assert: packet topics first equals: 'test/topic'! !

!MQTT5UnsubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testMultipleTopics
	| packet |
	packet := MQTTPacketUnsubscribe new.
	packet addTopic: 'topic1'.
	packet addTopic: 'topic2'.
	packet addTopic: 'topic3'.
	self assert: packet topics size equals: 3! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5DisconnectPacketTest"!

!MQTT5DisconnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPacketType
	| packet |
	packet := MQTTPacketDisconnect new.
	self assert: packet packetType equals: 14! !

!MQTT5DisconnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testDefaultReasonCode
	| packet |
	packet := MQTTPacketDisconnect new.
	self assert: packet reasonCode equals: 0! !

!MQTT5DisconnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReasonCode
	| packet |
	packet := MQTTPacketDisconnect new.
	packet reasonCode: 16r04.  "Disconnect with Will Message"
	self assert: packet reasonCode equals: 16r04! !

!MQTT5DisconnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testSessionExpiryInterval
	| packet |
	packet := MQTTPacketDisconnect new.
	packet sessionExpiryInterval: 0.
	self assert: packet sessionExpiryInterval equals: 0! !

!MQTT5DisconnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReasonString
	| packet |
	packet := MQTTPacketDisconnect new.
	packet reasonString: 'Normal shutdown'.
	self assert: packet reasonString equals: 'Normal shutdown'! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5AuthPacketTest"!

!MQTT5AuthPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPacketType
	| packet |
	packet := MQTTPacketAuth new.
	self assert: packet packetType equals: 15! !

!MQTT5AuthPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testDefaultReasonCode
	| packet |
	packet := MQTTPacketAuth new.
	self assert: packet reasonCode equals: 0! !

!MQTT5AuthPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testIsSuccess
	| packet |
	packet := MQTTPacketAuth new.
	packet reasonCode: 0.
	self assert: packet isSuccess! !

!MQTT5AuthPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testIsContinueAuthentication
	| packet |
	packet := MQTTPacketAuth new.
	packet reasonCode: 16r18.
	self assert: packet isContinueAuthentication.
	self deny: packet isSuccess! !

!MQTT5AuthPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testIsReauthenticate
	| packet |
	packet := MQTTPacketAuth new.
	packet reasonCode: 16r19.
	self assert: packet isReauthenticate! !

!MQTT5AuthPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAuthenticationMethod
	| packet |
	packet := MQTTPacketAuth new.
	packet authenticationMethod: 'SCRAM-SHA-256'.
	self assert: packet authenticationMethod equals: 'SCRAM-SHA-256'! !

!MQTT5AuthPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testAuthenticationData
	| packet data |
	data := #[1 2 3 4 5 6 7 8].
	packet := MQTTPacketAuth new.
	packet authenticationData: data.
	self assert: packet authenticationData equals: data! !

!MQTT5AuthPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReasonString
	| packet |
	packet := MQTTPacketAuth new.
	packet reasonString: 'Authentication required'.
	self assert: packet reasonString equals: 'Authentication required'! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTT5PubAckPacketTest"!

!MQTT5PubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPacketType
	| packet |
	packet := MQTTPacketPubAck new.
	self assert: packet packetType equals: 4! !

!MQTT5PubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testDefaultReasonCode
	| packet |
	packet := MQTTPacketPubAck new.
	self assert: packet reasonCode equals: 0! !

!MQTT5PubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testReasonCode
	| packet |
	packet := MQTTPacketPubAck new.
	packet reasonCode: 16r10.  "No matching subscribers"
	self assert: packet reasonCode equals: 16r10! !

!MQTT5PubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPubRecPacketType
	| packet |
	packet := MQTTPacketPubRec new.
	self assert: packet packetType equals: 5! !

!MQTT5PubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPubRelPacketType
	| packet |
	packet := MQTTPacketPubRel new.
	self assert: packet packetType equals: 6! !

!MQTT5PubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPubCompPacketType
	| packet |
	packet := MQTTPacketPubComp new.
	self assert: packet packetType equals: 7! !

!MQTT5PubAckPacketTest methodsFor: 'tests' stamp: 'JMM 1/21/2026 12:00'!
testPubRelFixedHeaderFlags
	| packet |
	packet := MQTTPacketPubRel new.
	self assert: packet fixedHeaderFlags equals: 2! !
