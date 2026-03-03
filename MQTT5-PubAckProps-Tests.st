TestCase subclass: #MQTT5PubAckPropsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-AllTests'!

!MQTT5PubAckPropsTest methodsFor: 'helpers' stamp: 'Simba 2/16/2026 07:30'!
encodeAndDecode: packetClass with: setupBlock
	"Encode a packet, then decode and return the decoded packet"
	| packet encoded stream remainingLength decoded |
	packet := packetClass new.
	packet packetID: 42.
	setupBlock value: packet.
	encoded := ByteArray streamContents: [ :s | packet encodeOn: s ].
	stream := ReadStream on: encoded.
	stream next. "skip fixed header"
	remainingLength := stream next. "simple 1-byte length"
	decoded := packetClass new.
	decoded decodeFrom: stream remainingLength: remainingLength.
	^ decoded.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - puback' stamp: 'Simba 2/16/2026 07:30'!
testPubAckReasonStringRoundTrip
	| decoded |
	decoded := self encodeAndDecode: MQTTPacketPubAck with: [ :p |
		p reasonCode: 16.
		p reasonString: 'No matching subscribers' ].
	self assert: decoded reasonCode equals: 16.
	self assert: decoded reasonString equals: 'No matching subscribers'.
	self assert: decoded packetID equals: 42.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - puback' stamp: 'Simba 2/16/2026 07:30'!
testPubAckUserPropertyRoundTrip
	| decoded ups |
	decoded := self encodeAndDecode: MQTTPacketPubAck with: [ :p |
		p reasonCode: 0.
		p addUserProperty: 'source' value: 'test-client'.
		p addUserProperty: 'timestamp' value: '2026-02-16' ].
	ups := decoded userProperties.
	self assert: ups size equals: 2.
	self assert: ups first key equals: 'source'.
	self assert: ups first value equals: 'test-client'.
	self assert: ups second key equals: 'timestamp'.
	self assert: ups second value equals: '2026-02-16'.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - puback' stamp: 'Simba 2/16/2026 07:30'!
testPubAckAllPropertiesRoundTrip
	| decoded ups |
	decoded := self encodeAndDecode: MQTTPacketPubAck with: [ :p |
		p reasonCode: 128.
		p reasonString: 'Unspecified error'.
		p addUserProperty: 'detail' value: 'something went wrong' ].
	self assert: decoded reasonCode equals: 128.
	self assert: decoded reasonString equals: 'Unspecified error'.
	ups := decoded userProperties.
	self assert: ups size equals: 1.
	self assert: ups first key equals: 'detail'.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - puback' stamp: 'Simba 2/16/2026 07:30'!
testPubAckMinimalRoundTrip
	"PUBACK with reason code 0 and no properties"
	| decoded |
	decoded := self encodeAndDecode: MQTTPacketPubAck with: [ :p |
		p reasonCode: 0 ].
	self assert: decoded reasonCode equals: 0.
	self assert: decoded reasonString isNil.
	self assert: decoded userProperties isEmpty.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - pubrec' stamp: 'Simba 2/16/2026 07:30'!
testPubRecReasonStringRoundTrip
	| decoded |
	decoded := self encodeAndDecode: MQTTPacketPubRec with: [ :p |
		p reasonCode: 16.
		p reasonString: 'No matching subscribers' ].
	self assert: decoded reasonCode equals: 16.
	self assert: decoded reasonString equals: 'No matching subscribers'.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - pubrec' stamp: 'Simba 2/16/2026 07:30'!
testPubRecUserPropertyRoundTrip
	| decoded ups |
	decoded := self encodeAndDecode: MQTTPacketPubRec with: [ :p |
		p reasonCode: 0.
		p addUserProperty: 'info' value: 'received' ].
	ups := decoded userProperties.
	self assert: ups size equals: 1.
	self assert: ups first key equals: 'info'.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - pubrec' stamp: 'Simba 2/16/2026 07:30'!
testPubRecAllPropertiesRoundTrip
	| decoded |
	decoded := self encodeAndDecode: MQTTPacketPubRec with: [ :p |
		p reasonCode: 131.
		p reasonString: 'Implementation specific error'.
		p addUserProperty: 'retry' value: 'false' ].
	self assert: decoded reasonCode equals: 131.
	self assert: decoded reasonString equals: 'Implementation specific error'.
	self assert: decoded userProperties size equals: 1.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - pubrel' stamp: 'Simba 2/16/2026 07:30'!
testPubRelReasonStringRoundTrip
	| decoded |
	decoded := self encodeAndDecode: MQTTPacketPubRel with: [ :p |
		p reasonCode: 146.
		p reasonString: 'Packet Identifier not found' ].
	self assert: decoded reasonCode equals: 146.
	self assert: decoded reasonString equals: 'Packet Identifier not found'.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - pubrel' stamp: 'Simba 2/16/2026 07:30'!
testPubRelUserPropertyRoundTrip
	| decoded ups |
	decoded := self encodeAndDecode: MQTTPacketPubRel with: [ :p |
		p reasonCode: 0.
		p addUserProperty: 'stage' value: 'release' ].
	ups := decoded userProperties.
	self assert: ups size equals: 1.
	self assert: ups first key equals: 'stage'.
	self assert: ups first value equals: 'release'.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - pubrel' stamp: 'Simba 2/16/2026 07:30'!
testPubRelAllPropertiesRoundTrip
	| decoded |
	decoded := self encodeAndDecode: MQTTPacketPubRel with: [ :p |
		p reasonCode: 146.
		p reasonString: 'ID not found'.
		p addUserProperty: 'k' value: 'v'.
		p addUserProperty: 'k2' value: 'v2' ].
	self assert: decoded reasonCode equals: 146.
	self assert: decoded reasonString equals: 'ID not found'.
	self assert: decoded userProperties size equals: 2.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - pubcomp' stamp: 'Simba 2/16/2026 07:30'!
testPubCompReasonStringRoundTrip
	| decoded |
	decoded := self encodeAndDecode: MQTTPacketPubComp with: [ :p |
		p reasonCode: 146.
		p reasonString: 'Packet Identifier not found' ].
	self assert: decoded reasonCode equals: 146.
	self assert: decoded reasonString equals: 'Packet Identifier not found'.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - pubcomp' stamp: 'Simba 2/16/2026 07:30'!
testPubCompUserPropertyRoundTrip
	| decoded ups |
	decoded := self encodeAndDecode: MQTTPacketPubComp with: [ :p |
		p reasonCode: 0.
		p addUserProperty: 'status' value: 'complete' ].
	ups := decoded userProperties.
	self assert: ups size equals: 1.
	self assert: ups first key equals: 'status'.
	self assert: ups first value equals: 'complete'.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - pubcomp' stamp: 'Simba 2/16/2026 07:30'!
testPubCompAllPropertiesRoundTrip
	| decoded |
	decoded := self encodeAndDecode: MQTTPacketPubComp with: [ :p |
		p reasonCode: 146.
		p reasonString: 'Not found'.
		p addUserProperty: 'a' value: 'b' ].
	self assert: decoded reasonCode equals: 146.
	self assert: decoded reasonString equals: 'Not found'.
	self assert: decoded userProperties size equals: 1.! !

!MQTT5PubAckPropsTest methodsFor: 'tests - cross packet' stamp: 'Simba 2/16/2026 07:30'!
testAllFourPacketTypesRoundTrip
	"Verify all four QoS acknowledgement packet types handle properties identically"
	#(MQTTPacketPubAck MQTTPacketPubRec MQTTPacketPubRel MQTTPacketPubComp) do: [ :clsName |
		| decoded |
		decoded := self encodeAndDecode: (Smalltalk at: clsName) with: [ :p |
			p reasonCode: 128.
			p reasonString: 'Error'.
			p addUserProperty: 'x' value: 'y' ].
		self assert: decoded reasonCode equals: 128 description: clsName , ' reasonCode'.
		self assert: decoded reasonString equals: 'Error' description: clsName , ' reasonString'.
		self assert: decoded userProperties size equals: 1 description: clsName , ' userProperties' ].! !

!MQTT5PubAckPropsTest methodsFor: 'tests - cross packet' stamp: 'Simba 2/16/2026 07:30'!
testMultipleUserPropertiesPreserveOrder
	"User properties should maintain insertion order across encode/decode"
	| decoded ups |
	decoded := self encodeAndDecode: MQTTPacketPubAck with: [ :p |
		p reasonCode: 0.
		p addUserProperty: 'first' value: '1'.
		p addUserProperty: 'second' value: '2'.
		p addUserProperty: 'third' value: '3' ].
	ups := decoded userProperties.
	self assert: ups size equals: 3.
	self assert: ups first key equals: 'first'.
	self assert: ups second key equals: 'second'.
	self assert: ups third key equals: 'third'.! !
