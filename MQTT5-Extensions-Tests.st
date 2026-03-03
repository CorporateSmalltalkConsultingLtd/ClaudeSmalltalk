TestCase subclass: #MQTT5ExtensionsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-AllTests'!

!MQTT5ExtensionsTest methodsFor: 'helpers' stamp: 'Simba 2/16/2026 07:50'!
decodeDisconnect: encoded
	| stream rl decoded |
	stream := ReadStream on: encoded.
	stream next. "fixed header"
	rl := stream next. "remaining length"
	decoded := MQTTPacketDisconnect new.
	decoded decodeFrom: stream remainingLength: rl.
	^ decoded.! !

!MQTT5ExtensionsTest methodsFor: 'helpers' stamp: 'Simba 2/16/2026 07:50'!
decodeAuth: encoded
	| stream rl decoded |
	stream := ReadStream on: encoded.
	stream next. "fixed header"
	rl := stream next. "remaining length"
	decoded := MQTTPacketAuth new.
	decoded decodeFrom: stream remainingLength: rl.
	^ decoded.! !

!MQTT5ExtensionsTest methodsFor: 'tests - disconnect user properties' stamp: 'Simba 2/16/2026 07:50'!
testDisconnectUserPropertyRoundTrip
	| packet encoded decoded |
	packet := MQTTPacketDisconnect new.
	packet reasonCode: 4.
	packet reasonString: 'Disconnect with Will Message'.
	packet addUserProperty: 'reason' value: 'testing'.
	packet addUserProperty: 'client' value: 'squeak'.
	encoded := ByteArray streamContents: [ :s | packet encodeOn: s ].
	decoded := self decodeDisconnect: encoded.
	self assert: decoded reasonCode equals: 4.
	self assert: decoded reasonString equals: 'Disconnect with Will Message'.
	self assert: decoded userProperties size equals: 2.
	self assert: decoded userProperties first key equals: 'reason'.
	self assert: decoded userProperties second key equals: 'client'.! !

!MQTT5ExtensionsTest methodsFor: 'tests - disconnect user properties' stamp: 'Simba 2/16/2026 07:50'!
testDisconnectSessionExpiryAndUserProperty
	| packet encoded decoded |
	packet := MQTTPacketDisconnect new.
	packet reasonCode: 0.
	packet sessionExpiryInterval: 300.
	packet addUserProperty: 'graceful' value: 'true'.
	encoded := ByteArray streamContents: [ :s | packet encodeOn: s ].
	decoded := self decodeDisconnect: encoded.
	self assert: decoded sessionExpiryInterval equals: 300.
	self assert: decoded userProperties size equals: 1.! !

!MQTT5ExtensionsTest methodsFor: 'tests - auth user properties' stamp: 'Simba 2/16/2026 07:50'!
testAuthUserPropertyRoundTrip
	| packet encoded decoded |
	packet := MQTTPacketAuth new.
	packet reasonCode: 24.
	packet authenticationMethod: 'SCRAM-SHA-256'.
	packet authenticationData: #[1 2 3 4].
	packet addUserProperty: 'step' value: '1'.
	encoded := ByteArray streamContents: [ :s | packet encodeOn: s ].
	decoded := self decodeAuth: encoded.
	self assert: decoded reasonCode equals: 24.
	self assert: decoded authenticationMethod equals: 'SCRAM-SHA-256'.
	self assert: decoded userProperties size equals: 1.
	self assert: decoded userProperties first key equals: 'step'.! !

!MQTT5ExtensionsTest methodsFor: 'tests - auth user properties' stamp: 'Simba 2/16/2026 07:50'!
testAuthReasonStringRoundTrip
	| packet encoded decoded |
	packet := MQTTPacketAuth new.
	packet reasonCode: 0.
	packet reasonString: 'Authentication successful'.
	encoded := ByteArray streamContents: [ :s | packet encodeOn: s ].
	decoded := self decodeAuth: encoded.
	self assert: decoded reasonCode equals: 0.
	self assert: decoded reasonString equals: 'Authentication successful'.! !

!MQTT5ExtensionsTest methodsFor: 'tests - publish properties api' stamp: 'Simba 2/16/2026 07:50'!
testPublishUserPropertyAccessors
	| packet |
	packet := MQTTPacketPublish new.
	packet topic: 'test'.
	packet message: 'hi' asByteArray.
	packet qos: 0.
	packet addUserProperty: 'key1' value: 'val1'.
	packet addUserProperty: 'key2' value: 'val2'.
	self assert: packet userProperties size equals: 2.
	self assert: packet userProperties first key equals: 'key1'.
	self assert: packet userProperties second value equals: 'val2'.! !

!MQTT5ExtensionsTest methodsFor: 'tests - publish properties api' stamp: 'Simba 2/16/2026 07:50'!
testPublishUserPropertyRoundTrip
	| packet encoded decoded |
	packet := MQTTPacketPublish new.
	packet topic: 'test/up'.
	packet message: 'data' asByteArray.
	packet qos: 1.
	packet messageID: 10.
	packet addUserProperty: 'source' value: 'squeak'.
	packet addUserProperty: 'version' value: '5.0'.
	encoded := ByteArray streamContents: [ :s | packet encodeOn: s ].
	decoded := MQTTPacket readFrom: (ReadStream on: encoded).
	self assert: decoded userProperties size equals: 2.
	self assert: decoded userProperties first key equals: 'source'.
	self assert: decoded userProperties first value equals: 'squeak'.! !

!MQTT5ExtensionsTest methodsFor: 'tests - publish properties api' stamp: 'Simba 2/16/2026 07:50'!
testPublishAllPropertiesViaBlock
	| packet |
	packet := MQTTPacketPublish new.
	packet topic: 'test/block'.
	packet message: 'payload' asByteArray.
	packet qos: 1.
	[:p |
		p payloadFormatIndicator: 1.
		p contentType: 'application/json'.
		p messageExpiryInterval: 3600.
		p responseTopic: 'reply/here'.
		p correlationData: 'corr-1' asByteArray.
		p addUserProperty: 'x' value: 'y'.
	] value: packet.
	self assert: packet payloadFormatIndicator equals: 1.
	self assert: packet contentType equals: 'application/json'.
	self assert: packet messageExpiryInterval equals: 3600.
	self assert: packet responseTopic equals: 'reply/here'.
	self assert: packet correlationData equals: 'corr-1' asByteArray.
	self assert: packet userProperties size equals: 1.! !

!MQTT5ExtensionsTest methodsFor: 'tests - will properties' stamp: 'Simba 2/16/2026 07:50'!
testWillDelayIntervalAccessor
	| packet |
	packet := MQTTPacketConnect new.
	packet willDelayInterval: 60.
	self assert: packet willDelayInterval equals: 60.! !

!MQTT5ExtensionsTest methodsFor: 'tests - will properties' stamp: 'Simba 2/16/2026 07:50'!
testWillPayloadFormatIndicatorAccessor
	| packet |
	packet := MQTTPacketConnect new.
	packet willPayloadFormatIndicator: 1.
	self assert: packet willPayloadFormatIndicator equals: 1.! !

!MQTT5ExtensionsTest methodsFor: 'tests - will properties' stamp: 'Simba 2/16/2026 07:50'!
testWillMessageExpiryIntervalAccessor
	| packet |
	packet := MQTTPacketConnect new.
	packet willMessageExpiryInterval: 7200.
	self assert: packet willMessageExpiryInterval equals: 7200.! !

!MQTT5ExtensionsTest methodsFor: 'tests - will properties' stamp: 'Simba 2/16/2026 07:50'!
testWillContentTypeAccessor
	| packet |
	packet := MQTTPacketConnect new.
	packet willContentType: 'text/plain'.
	self assert: packet willContentType equals: 'text/plain'.! !

!MQTT5ExtensionsTest methodsFor: 'tests - will properties' stamp: 'Simba 2/16/2026 07:50'!
testWillResponseTopicAccessor
	| packet |
	packet := MQTTPacketConnect new.
	packet willResponseTopic: 'will/response'.
	self assert: packet willResponseTopic equals: 'will/response'.! !

!MQTT5ExtensionsTest methodsFor: 'tests - will properties' stamp: 'Simba 2/16/2026 07:50'!
testWillUserPropertyAccessor
	| packet |
	packet := MQTTPacketConnect new.
	packet addWillUserProperty: 'reason' value: 'crash'.
	self assert: packet willProperties notEmpty.! !

!MQTT5ExtensionsTest methodsFor: 'tests - will properties' stamp: 'Simba 2/16/2026 07:50'!
testWillPropertiesEncodeInConnect
	| packet encoded |
	packet := MQTTPacketConnect new.
	packet protocolLevel: 5.
	packet keepAliveTime: 60.
	packet cleanSessionFlag: true.
	packet willTopic: 'will/test' message: 'goodbye' retain: false qos: 1.
	packet willDelayInterval: 30.
	packet willContentType: 'text/plain'.
	encoded := ByteArray streamContents: [ :s | packet encodeOn: s ].
	self assert: encoded size > 20.
	self assert: (encoded first bitShift: -4) equals: 1.! !
