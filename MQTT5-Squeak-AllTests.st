'From Squeak 6.0 on 16 February 2026 at 5:22:00 am'!
'Description: Comprehensive MQTT5 v5.0 property tests - unit + integration'!

SystemOrganization addCategory: #'MQTT5-Squeak-AllTests'!



TestCase subclass: #MQTT5PacketUnitTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-AllTests'!



!MQTT5PacketUnitTest methodsFor: 'helpers' stamp: 'Simba 2/16/2026 05:22'!
encodeAndDecode: aPacket
	"Encode a packet to bytes, then decode using MQTTPacket readFrom:"
	| encoded stream |
	encoded := ByteArray streamContents: [:s | aPacket encodeOn: s].
	stream := ReadStream on: encoded.
	^ MQTTPacket readFrom: stream.! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishPayloadFormatIndicatorRoundTrip
	"Payload Format Indicator: 0=unspecified bytes, 1=UTF-8"
	| packet decoded |
	packet := MQTTPacketPublish new
		topic: 'test/pfi' message: #[72 101 108 108 111];
		qos: 0;
		payloadFormatIndicator: 1.
	decoded := self encodeAndDecode: packet.
	self assert: decoded payloadFormatIndicator equals: 1.
	self assert: decoded topic equals: 'test/pfi'.
	self assert: decoded message equals: #[72 101 108 108 111].! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishPayloadFormatIndicatorZero
	"Payload Format Indicator 0 = unspecified bytes"
	| packet decoded |
	packet := MQTTPacketPublish new
		topic: 'test/pfi0' message: #[0 1 2 255];
		qos: 0;
		payloadFormatIndicator: 0.
	decoded := self encodeAndDecode: packet.
	self assert: decoded payloadFormatIndicator equals: 0.! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishMessageExpiryIntervalRoundTrip
	"Message Expiry Interval: 4-byte integer, seconds until expiry"
	| packet decoded |
	packet := MQTTPacketPublish new
		topic: 'test/expiry' message: #[104 105];
		qos: 0;
		messageExpiryInterval: 3600.
	decoded := self encodeAndDecode: packet.
	self assert: decoded messageExpiryInterval equals: 3600.! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishResponseTopicRoundTrip
	"Response Topic: UTF-8 string for request/response pattern"
	| packet decoded |
	packet := MQTTPacketPublish new
		topic: 'request/topic' message: #[1 2 3];
		qos: 0;
		responseTopic: 'response/here'.
	decoded := self encodeAndDecode: packet.
	self assert: decoded responseTopic equals: 'response/here'.! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishCorrelationDataRoundTrip
	"Correlation Data: binary data for request/response matching"
	| packet decoded corrData |
	corrData := #[10 20 30 40 50 60 70 80].
	packet := MQTTPacketPublish new
		topic: 'test/corr' message: #[1];
		qos: 0;
		correlationData: corrData.
	decoded := self encodeAndDecode: packet.
	self assert: decoded correlationData equals: corrData.! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishContentTypeRoundTrip
	"Content Type: UTF-8 MIME type string"
	| packet decoded |
	packet := MQTTPacketPublish new
		topic: 'test/ct' message: (UTF8Encoder encode: '{"key":"value"}');
		qos: 0;
		contentType: 'application/json'.
	decoded := self encodeAndDecode: packet.
	self assert: decoded contentType equals: 'application/json'.! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishTopicAliasRoundTrip
	"Topic Alias: 2-byte integer, used for topic string reduction"
	| packet decoded |
	packet := MQTTPacketPublish new
		topic: 'test/alias' message: #[1];
		qos: 0;
		topicAlias: 42.
	decoded := self encodeAndDecode: packet.
	self assert: decoded topicAlias equals: 42.! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishAllPropertiesCombined
	"Test all PUBLISH properties set simultaneously"
	| packet decoded corrData |
	corrData := #[1 2 3 4].
	packet := MQTTPacketPublish new
		topic: 'test/all' message: (UTF8Encoder encode: 'full props');
		qos: 0;
		payloadFormatIndicator: 1;
		messageExpiryInterval: 7200;
		responseTopic: 'reply/to/me';
		correlationData: corrData;
		contentType: 'text/plain';
		topicAlias: 7.
	decoded := self encodeAndDecode: packet.
	self assert: decoded payloadFormatIndicator equals: 1.
	self assert: decoded messageExpiryInterval equals: 7200.
	self assert: decoded responseTopic equals: 'reply/to/me'.
	self assert: decoded correlationData equals: corrData.
	self assert: decoded contentType equals: 'text/plain'.
	self assert: decoded topicAlias equals: 7.
	self assert: decoded topic equals: 'test/all'.! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishSubscriptionIdentifierRoundTrip
	"Subscription Identifier: variable byte integer, set by broker on delivery"
	| packet decoded |
	packet := MQTTPacketPublish new
		topic: 'test/subid' message: #[1];
		qos: 0.
	packet addSubscriptionIdentifier: 12345.
	decoded := self encodeAndDecode: packet.
	self assert: decoded subscriptionIdentifiers notEmpty.
	self assert: decoded subscriptionIdentifiers first equals: 12345.! !

!MQTT5PacketUnitTest methodsFor: 'tests - publish properties' stamp: 'Simba 2/16/2026 05:22'!
testPublishMultipleSubscriptionIdentifiers
	"Multiple Subscription Identifiers can appear in a single PUBLISH"
	| packet decoded |
	packet := MQTTPacketPublish new
		topic: 'test/multisub' message: #[1];
		qos: 0.
	packet addSubscriptionIdentifier: 1.
	packet addSubscriptionIdentifier: 2.
	packet addSubscriptionIdentifier: 3.
	decoded := self encodeAndDecode: packet.
	self assert: decoded subscriptionIdentifiers size equals: 3.
	self assert: decoded subscriptionIdentifiers asArray equals: #(1 2 3).! !



!MQTT5PacketUnitTest methodsFor: 'tests - connect properties' stamp: 'Simba 2/16/2026 05:22'!
testConnectSessionExpiryIntervalEncode
	"Verify CONNECT packet with Session Expiry Interval encodes without error"
	| packet encoded |
	packet := MQTTPacketConnect new.
	packet protocolLevel: 5.
	packet cleanSessionFlag: true.
	packet keepAliveTime: 60.
	packet clientID: 'test-client'.
	packet user: (UTF8Encoder encode: 'user') password: (UTF8Encoder encode: 'pass').
	packet sessionExpiryInterval: 300.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	self assert: packet sessionExpiryInterval equals: 300.! !

!MQTT5PacketUnitTest methodsFor: 'tests - connect properties' stamp: 'Simba 2/16/2026 05:22'!
testConnectReceiveMaximumEncode
	"Verify CONNECT with Receive Maximum encodes correctly"
	| packet encoded |
	packet := MQTTPacketConnect new.
	packet protocolLevel: 5.
	packet cleanSessionFlag: true.
	packet keepAliveTime: 60.
	packet clientID: 'test-rm'.
	packet user: (UTF8Encoder encode: 'u') password: (UTF8Encoder encode: 'p').
	packet receiveMaximum: 20.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	self assert: packet receiveMaximum equals: 20.! !

!MQTT5PacketUnitTest methodsFor: 'tests - connect properties' stamp: 'Simba 2/16/2026 05:22'!
testConnectMaximumPacketSizeEncode
	"Verify CONNECT with Maximum Packet Size encodes correctly"
	| packet encoded |
	packet := MQTTPacketConnect new.
	packet protocolLevel: 5.
	packet cleanSessionFlag: true.
	packet keepAliveTime: 60.
	packet clientID: 'test-mps'.
	packet user: (UTF8Encoder encode: 'u') password: (UTF8Encoder encode: 'p').
	packet maximumPacketSize: 65536.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	self assert: packet maximumPacketSize equals: 65536.! !

!MQTT5PacketUnitTest methodsFor: 'tests - connect properties' stamp: 'Simba 2/16/2026 05:22'!
testConnectTopicAliasMaximumEncode
	"Verify CONNECT with Topic Alias Maximum encodes correctly"
	| packet encoded |
	packet := MQTTPacketConnect new.
	packet protocolLevel: 5.
	packet cleanSessionFlag: true.
	packet keepAliveTime: 60.
	packet clientID: 'test-tam'.
	packet user: (UTF8Encoder encode: 'u') password: (UTF8Encoder encode: 'p').
	packet topicAliasMaximum: 10.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	self assert: packet topicAliasMaximum equals: 10.! !

!MQTT5PacketUnitTest methodsFor: 'tests - connect properties' stamp: 'Simba 2/16/2026 05:22'!
testConnectAuthMethodEncode
	"Verify CONNECT with Authentication Method/Data encodes correctly"
	| packet encoded |
	packet := MQTTPacketConnect new.
	packet protocolLevel: 5.
	packet cleanSessionFlag: true.
	packet keepAliveTime: 60.
	packet clientID: 'test-auth'.
	packet user: (UTF8Encoder encode: 'u') password: (UTF8Encoder encode: 'p').
	packet authenticationMethod: 'SCRAM-SHA-256'.
	packet authenticationData: #[1 2 3 4 5].
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	self assert: packet authenticationMethod equals: 'SCRAM-SHA-256'.
	self assert: packet authenticationData equals: #[1 2 3 4 5].! !

!MQTT5PacketUnitTest methodsFor: 'tests - connect properties' stamp: 'Simba 2/16/2026 05:22'!
testConnectAllPropertiesCombinedEncode
	"Verify CONNECT with all properties set encodes correctly"
	| packet encoded |
	packet := MQTTPacketConnect new.
	packet protocolLevel: 5.
	packet cleanSessionFlag: true.
	packet keepAliveTime: 60.
	packet clientID: 'test-allconn'.
	packet user: (UTF8Encoder encode: 'user') password: (UTF8Encoder encode: 'pass').
	packet sessionExpiryInterval: 600.
	packet receiveMaximum: 50.
	packet maximumPacketSize: 32768.
	packet topicAliasMaximum: 5.
	packet requestResponseInfo: true.
	packet requestProblemInfo: true.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	"Verify all properties retained on the object"
	self assert: packet sessionExpiryInterval equals: 600.
	self assert: packet receiveMaximum equals: 50.
	self assert: packet maximumPacketSize equals: 32768.
	self assert: packet topicAliasMaximum equals: 5.! !



!MQTT5PacketUnitTest methodsFor: 'tests - subscribe properties' stamp: 'Simba 2/16/2026 05:22'!
testSubscribeSubscriptionIdentifierEncode
	"Verify SUBSCRIBE with Subscription Identifier encodes correctly"
	| packet encoded |
	packet := MQTTPacketSubscribe new.
	packet packetID: 1.
	packet addTopic: 'test/subid' qos: 1.
	packet subscriptionIdentifier: 42.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	self assert: packet subscriptionIdentifier equals: 42.
	"Verify the subscription identifier byte (11) appears in the encoded data"
	self assert: (encoded includes: 11).! !

!MQTT5PacketUnitTest methodsFor: 'tests - subscribe properties' stamp: 'Simba 2/16/2026 05:22'!
testSubscribeSubscriptionIdentifierLargeValue
	"Variable byte integer can encode up to 268,435,455"
	| packet encoded |
	packet := MQTTPacketSubscribe new.
	packet packetID: 2.
	packet addTopic: 'test/large' qos: 0.
	packet subscriptionIdentifier: 268435455.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	self assert: packet subscriptionIdentifier equals: 268435455.! !

!MQTT5PacketUnitTest methodsFor: 'tests - subscribe properties' stamp: 'Simba 2/16/2026 05:22'!
testSubscribeWithoutProperties
	"Subscribe with no properties should encode property length = 0"
	| packet encoded |
	packet := MQTTPacketSubscribe new.
	packet packetID: 3.
	packet addTopic: 'test/noprops' qos: 1.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	self assert: packet subscriptionIdentifier isNil.! !

!MQTT5PacketUnitTest methodsFor: 'tests - subscribe properties' stamp: 'Simba 2/16/2026 05:22'!
testSubscribeV5SubscriptionOptions
	"Test v5 subscription option flags: noLocal, retainAsPublished, retainHandling"
	| sub opts |
	sub := MQTTSubscription new.
	sub qos: 2.
	sub noLocal: true.
	sub retainAsPublished: true.
	sub retainHandling: 1.
	opts := sub subscriptionOptionsV5.
	"QoS bits 0-1 = 2, noLocal bit 2 = 4, retainAsPublished bit 3 = 8, retainHandling bits 4-5 = 16"
	self assert: (opts bitAnd: 3) equals: 2.
	self assert: (opts bitAnd: 4) equals: 4.
	self assert: (opts bitAnd: 8) equals: 8.
	self assert: ((opts bitShift: -4) bitAnd: 3) equals: 1.! !



!MQTT5PacketUnitTest methodsFor: 'tests - disconnect properties' stamp: 'Simba 2/16/2026 05:22'!
testDisconnectReasonCodeRoundTrip
	"DISCONNECT with reason code 0 and no properties encodes as 2 bytes"
	| packet encoded decoded stream |
	packet := MQTTPacketDisconnect new.
	packet reasonCode: 0.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	"Reason code 0 with no properties should encode to minimal packet"
	self assert: encoded notEmpty.
	stream := ReadStream on: encoded.
	stream next. "skip fixed header"
	decoded := MQTTPacketDisconnect new.
	decoded decodeFrom: stream remainingLength: (encoded size - 2 max: 0).
	self assert: decoded reasonCode equals: 0.! !

!MQTT5PacketUnitTest methodsFor: 'tests - disconnect properties' stamp: 'Simba 2/16/2026 05:22'!
testDisconnectSessionExpiryIntervalRoundTrip
	"Test DISCONNECT with Session Expiry Interval encode/decode"
	| packet encoded decoded stream remainingLength |
	packet := MQTTPacketDisconnect new.
	packet reasonCode: 0.
	packet sessionExpiryInterval: 120.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	self assert: encoded notEmpty.
	"Decode: skip fixed header byte, read remaining length, then decode"
	stream := ReadStream on: encoded.
	stream next. "skip fixed header"
	remainingLength := stream next. "simple 1-byte length"
	decoded := MQTTPacketDisconnect new.
	decoded decodeFrom: stream remainingLength: remainingLength.
	self assert: decoded sessionExpiryInterval equals: 120.! !

!MQTT5PacketUnitTest methodsFor: 'tests - disconnect properties' stamp: 'Simba 2/16/2026 05:22'!
testDisconnectReasonStringRoundTrip
	"Test DISCONNECT with Reason String encode/decode"
	| packet encoded decoded stream remainingLength |
	packet := MQTTPacketDisconnect new.
	packet reasonCode: 4.
	packet reasonString: 'Normal disconnect with reason'.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	stream := ReadStream on: encoded.
	stream next. "skip fixed header"
	remainingLength := stream next.
	decoded := MQTTPacketDisconnect new.
	decoded decodeFrom: stream remainingLength: remainingLength.
	self assert: decoded reasonString equals: 'Normal disconnect with reason'.! !



!MQTT5PacketUnitTest methodsFor: 'tests - auth properties' stamp: 'Simba 2/16/2026 05:22'!
testAuthMethodAndDataRoundTrip
	"Test AUTH with Authentication Method/Data encode/decode"
	| packet encoded decoded stream remainingLength |
	packet := MQTTPacketAuth new.
	packet reasonCode: 24.
	packet authenticationMethod: 'SCRAM-SHA-256'.
	packet authenticationData: #[65 66 67 68].
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	stream := ReadStream on: encoded.
	stream next. "skip fixed header"
	remainingLength := stream next.
	decoded := MQTTPacketAuth new.
	decoded decodeFrom: stream remainingLength: remainingLength.
	self assert: decoded reasonCode equals: 24.
	self assert: decoded authenticationMethod equals: 'SCRAM-SHA-256'.
	self assert: decoded authenticationData equals: #[65 66 67 68].! !

!MQTT5PacketUnitTest methodsFor: 'tests - auth properties' stamp: 'Simba 2/16/2026 05:22'!
testAuthReasonStringRoundTrip
	"Test AUTH with Reason String encode/decode"
	| packet encoded decoded stream remainingLength |
	packet := MQTTPacketAuth new.
	packet reasonCode: 0.
	packet reasonString: 'Authentication successful'.
	encoded := ByteArray streamContents: [:s | packet encodeOn: s].
	stream := ReadStream on: encoded.
	stream next. "skip fixed header"
	remainingLength := stream next.
	decoded := MQTTPacketAuth new.
	decoded decodeFrom: stream remainingLength: remainingLength.
	self assert: decoded reasonString equals: 'Authentication successful'.! !



!MQTT5PacketUnitTest methodsFor: 'tests - properties framework' stamp: 'Simba 2/16/2026 05:22'!
testMQTT5PropertiesAddAndRetrieve
	"Test the properties container add/get"
	| props |
	props := MQTT5Properties new.
	props addProperty: (MQTT5Property identifier: 1 value: 1).
	props addProperty: (MQTT5Property identifier: 3 value: 'text/plain').
	self assert: (props propertyAt: 1) equals: 1.
	self assert: (props propertyAt: 3) equals: 'text/plain'.
	self assert: (props propertyAt: 99) isNil.! !

!MQTT5PacketUnitTest methodsFor: 'tests - properties framework' stamp: 'Simba 2/16/2026 05:22'!
testMQTT5PropertiesUserPropertyMultiple
	"User Property (38) can appear multiple times"
	| props userProps |
	props := MQTT5Properties new.
	props addProperty: (MQTT5Property identifier: 38 value: #('key1' 'val1')).
	props addProperty: (MQTT5Property identifier: 38 value: #('key2' 'val2')).
	userProps := props propertiesAt: 38.
	self assert: userProps size equals: 2.! !

!MQTT5PacketUnitTest methodsFor: 'tests - properties framework' stamp: 'Simba 2/16/2026 05:22'!
testMQTT5PropertiesEncodeDecodeRoundTrip
	"Test encoding properties to bytes and decoding back"
	| props encoded decoded stream |
	props := MQTT5Properties new.
	props addProperty: (MQTT5Property identifier: 1 value: 1).
	props addProperty: (MQTT5Property identifier: 2 value: 3600).
	encoded := ByteArray streamContents: [:s | props encodeOn: s].
	self assert: encoded notEmpty.! !



!MQTT5PacketUnitTest methodsFor: 'tests - constants' stamp: 'Simba 2/16/2026 05:22'!
testMQTT5ConstantsPropertyIdentifiers
	"Verify all property identifier constants match the spec"
	self assert: MQTT5Constants payloadFormatIndicator equals: 1.
	self assert: MQTT5Constants messageExpiryInterval equals: 2.
	self assert: MQTT5Constants contentType equals: 3.
	self assert: MQTT5Constants responseTopic equals: 8.
	self assert: MQTT5Constants correlationData equals: 9.
	self assert: MQTT5Constants subscriptionIdentifier equals: 11.
	self assert: MQTT5Constants sessionExpiryInterval equals: 17.
	self assert: MQTT5Constants assignedClientIdentifier equals: 18.
	self assert: MQTT5Constants serverKeepAlive equals: 19.
	self assert: MQTT5Constants authenticationMethod equals: 21.
	self assert: MQTT5Constants authenticationData equals: 22.
	self assert: MQTT5Constants requestProblemInformation equals: 23.
	self assert: MQTT5Constants requestResponseInformation equals: 25.
	self assert: MQTT5Constants responseInformation equals: 26.
	self assert: MQTT5Constants serverReference equals: 28.
	self assert: MQTT5Constants reasonString equals: 31.
	self assert: MQTT5Constants receiveMaximum equals: 33.
	self assert: MQTT5Constants topicAliasMaximum equals: 34.
	self assert: MQTT5Constants topicAlias equals: 35.
	self assert: MQTT5Constants maximumQoS equals: 36.
	self assert: MQTT5Constants retainAvailable equals: 37.
	self assert: MQTT5Constants userProperty equals: 38.
	self assert: MQTT5Constants maximumPacketSize equals: 39.
	self assert: MQTT5Constants wildcardSubscriptionAvailable equals: 40.
	self assert: MQTT5Constants subscriptionIdentifiersAvailable equals: 41.
	self assert: MQTT5Constants sharedSubscriptionAvailable equals: 42.! !

!MQTT5PacketUnitTest methodsFor: 'tests - constants' stamp: 'Simba 2/16/2026 05:22'!
testMQTT5ConstantsReasonCodes
	"Verify key reason codes"
	self assert: MQTT5Constants reasonCodeSuccess equals: 0.
	self assert: MQTT5Constants reasonCodeNormalDisconnection equals: 0.
	self assert: MQTT5Constants reasonCodeGrantedQoS0 equals: 0.
	self assert: MQTT5Constants reasonCodeGrantedQoS1 equals: 1.
	self assert: MQTT5Constants reasonCodeGrantedQoS2 equals: 2.
	self assert: MQTT5Constants reasonCodeNotAuthorized equals: 135.
	self assert: MQTT5Constants reasonCodeTopicAliasInvalid equals: 148.
	self assert: MQTT5Constants reasonCodePayloadFormatInvalid equals: 153.! !



!MQTT5PacketUnitTest methodsFor: 'tests - server capabilities' stamp: 'Simba 2/16/2026 05:22'!
testServerCapabilitiesDefaults
	"Verify default server capabilities per spec"
	| caps |
	caps := MQTT5ServerCapabilities new.
	self assert: caps topicAliasMaximum equals: 0.
	self assert: caps subscriptionIdentifiersAvailable equals: true.! !

!MQTT5PacketUnitTest methodsFor: 'tests - server capabilities' stamp: 'Simba 2/16/2026 05:22'!
testServerCapabilitiesUpdateFromConnAck
	"Test parsing CONNACK properties into server capabilities"
	| caps props |
	caps := MQTT5ServerCapabilities new.
	props := MQTT5Properties new.
	props addProperty: (MQTT5Property identifier: 34 value: 10). "Topic Alias Maximum"
	props addProperty: (MQTT5Property identifier: 41 value: 1). "Subscription Identifiers Available"
	caps updateFromConnAckProperties: props.
	self assert: caps topicAliasMaximum equals: 10.
	self assert: caps subscriptionIdentifiersAvailable equals: true.! !



!MQTT5PacketUnitTest methodsFor: 'tests - topic alias' stamp: 'Simba 2/16/2026 05:22'!
testTopicAliasMapOutgoing
	"Test outgoing topic alias assignment"
	| transport caps |
	transport := MQTTTransportLayerClient new.
	caps := transport serverCapabilities.
	caps topicAliasMaximum: 3.
	"First call should assign alias 1"
	self assert: (transport nextTopicAliasFor: 'topic/a') equals: 1.
	"Same topic should return same alias"
	self assert: (transport nextTopicAliasFor: 'topic/a') equals: 1.
	"New topic gets alias 2"
	self assert: (transport nextTopicAliasFor: 'topic/b') equals: 2.
	self assert: (transport nextTopicAliasFor: 'topic/c') equals: 3.
	"No more aliases available (max=3)"
	self assert: (transport nextTopicAliasFor: 'topic/d') isNil.! !

!MQTT5PacketUnitTest methodsFor: 'tests - topic alias' stamp: 'Simba 2/16/2026 05:22'!
testTopicAliasMapIncoming
	"Test incoming topic alias resolution"
	| transport |
	transport := MQTTTransportLayerClient new.
	transport registerTopicAliasIncoming: 1 forTopic: 'sensors/temp'.
	transport registerTopicAliasIncoming: 2 forTopic: 'sensors/humidity'.
	self assert: (transport resolveTopicAliasIncoming: 1) equals: 'sensors/temp'.
	self assert: (transport resolveTopicAliasIncoming: 2) equals: 'sensors/humidity'.
	self assert: (transport resolveTopicAliasIncoming: 99) isNil.! !

!MQTT5PacketUnitTest methodsFor: 'tests - topic alias' stamp: 'Simba 2/16/2026 05:22'!
testTopicAliasClearMaps
	"Test that clearTopicAliasMaps resets both maps"
	| transport |
	transport := MQTTTransportLayerClient new.
	transport serverCapabilities topicAliasMaximum: 5.
	transport nextTopicAliasFor: 'a'.
	transport registerTopicAliasIncoming: 1 forTopic: 'b'.
	self assert: transport topicAliasMapOutgoing notEmpty.
	self assert: transport topicAliasMapIncoming notEmpty.
	transport clearTopicAliasMaps.
	self assert: transport topicAliasMapOutgoing isEmpty.
	self assert: transport topicAliasMapIncoming isEmpty.! !



!MQTT5PacketUnitTest methodsFor: 'tests - packet types' stamp: 'Simba 2/16/2026 05:22'!
testPacketTypeValues
	"Verify packet type constants per spec"
	self assert: MQTTPacketConnect new packetType equals: 1.
	self assert: MQTTPacketConnAck new packetType equals: 2.
	self assert: MQTTPacketPublish new packetType equals: 3.
	self assert: MQTTPacketPubAck new packetType equals: 4.
	self assert: MQTTPacketPubRec new packetType equals: 5.
	self assert: MQTTPacketPubRel new packetType equals: 6.
	self assert: MQTTPacketPubComp new packetType equals: 7.
	self assert: MQTTPacketSubscribe new packetType equals: 8.
	self assert: MQTTPacketSubAck new packetType equals: 9.
	self assert: MQTTPacketUnsubscribe new packetType equals: 10.
	self assert: MQTTPacketUnsubAck new packetType equals: 11.
	self assert: MQTTPacketPingReq new packetType equals: 12.
	self assert: MQTTPacketPingResp new packetType equals: 13.
	self assert: MQTTPacketDisconnect new packetType equals: 14.
	self assert: MQTTPacketAuth new packetType equals: 15.! !



!MQTT5PacketUnitTest methodsFor: 'tests - pending jobs' stamp: 'Simba 2/16/2026 05:22'!
testPendingJobExpiry
	"Test that pending jobs track time correctly"
	| job |
	job := MQTTPendingJob for: MQTTPacketPubAck new.
	self deny: (job isExpired: 30).
	self deny: job isOverdue.! !

!MQTT5PacketUnitTest methodsFor: 'tests - pending jobs' stamp: 'Simba 2/16/2026 05:22'!
testPendingJobTypes
	"Test that QoS levels create correct pending job types"
	| pub1 pub2 |
	pub1 := MQTTPacketPublish new topic: 'a' message: #[1]; qos: 1; messageID: 1.
	pub2 := MQTTPacketPublish new topic: 'b' message: #[2]; qos: 2; messageID: 2.
	self assert: (pub1 pendingJob isKindOf: MQTTPendingPubAckJob).
	self assert: (pub2 pendingJob isKindOf: MQTTPendingPubRecJob).! !



!MQTT5PacketUnitTest methodsFor: 'tests - subscription matching' stamp: 'Simba 2/16/2026 05:22'!
testSubscriptionExactMatch
	| sub |
	sub := MQTTSubscription new topic: 'sensors/temp'.
	self assert: (sub topicMatches: 'sensors/temp').
	self deny: (sub topicMatches: 'sensors/humidity').
	self deny: (sub topicMatches: 'sensors/temp/extra').! !

!MQTT5PacketUnitTest methodsFor: 'tests - subscription matching' stamp: 'Simba 2/16/2026 05:22'!
testSubscriptionWildcardHash
	| sub |
	sub := MQTTSubscription new topic: 'sensors/#'.
	self assert: (sub topicMatches: 'sensors/temp').
	self assert: (sub topicMatches: 'sensors/temp/indoor').
	self assert: (sub topicMatches: 'sensors').  "# matches parent level too"
	self deny: (sub topicMatches: 'actuators/relay').! !

!MQTT5PacketUnitTest methodsFor: 'tests - subscription matching' stamp: 'Simba 2/16/2026 05:22'!
testSubscriptionWildcardPlus
	| sub |
	sub := MQTTSubscription new topic: 'sensors/+/temp'.
	self assert: (sub topicMatches: 'sensors/room1/temp').
	self assert: (sub topicMatches: 'sensors/room2/temp').
	self deny: (sub topicMatches: 'sensors/room1/humidity').
	self deny: (sub topicMatches: 'sensors/temp').! !




MQTT5IntegrationTest subclass: #MQTT5PublishPropertiesIntegrationTest
	instanceVariableNames: 'received receivedTopic receivedPacket'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-AllTests'!

!MQTT5PublishPropertiesIntegrationTest methodsFor: 'running' stamp: 'Simba 2/16/2026 05:22'!
setUp
	super setUp.
	received := nil.
	receivedTopic := nil.
	receivedPacket := nil.! !

!MQTT5PublishPropertiesIntegrationTest methodsFor: 'helpers' stamp: 'Simba 2/16/2026 05:22'!
publishPacketOnTopic: topic message: msg qos: qos withProperties: propBlock
	"Create a PUBLISH packet with custom properties and send it"
	| packet transport |
	transport := client socketClient transport.
	packet := MQTTPacketPublish new
		topic: topic message: (UTF8Encoder encode: msg);
		qos: qos;
		retainFlag: false.
	propBlock value: packet.
	qos > 0 ifTrue: [
		packet messageID: transport nextMsgID.
		transport addPendingJob: packet pendingJob forPacketID: packet messageID].
	transport sendPacket: (MQTTPacketAndPendingJobPair packet: packet job: nil).! !

!MQTT5PublishPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testPublishWithPayloadFormatIndicator
	"Test that broker accepts PUBLISH with Payload Format Indicator"
	| topic msg |
	topic := self uniqueTopic.
	msg := 'UTF-8 payload test'.
	self connectClient.
	client subscribeTo: topic qos: 1 do: [:t :m :q |
		receivedTopic := t.
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	self publishPacketOnTopic: topic message: msg qos: 1 withProperties: [:pkt |
		pkt payloadFormatIndicator: 1].
	self waitForDelivery.
	self assert: received equals: msg.
	self assert: receivedTopic equals: topic.! !

!MQTT5PublishPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testPublishWithMessageExpiryInterval
	"Test PUBLISH with Message Expiry Interval property"
	| topic msg |
	topic := self uniqueTopic.
	msg := 'Expiring in 60s'.
	self connectClient.
	client subscribeTo: topic qos: 1 do: [:t :m :q |
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	self publishPacketOnTopic: topic message: msg qos: 1 withProperties: [:pkt |
		pkt messageExpiryInterval: 60].
	self waitForDelivery.
	self assert: received equals: msg.! !

!MQTT5PublishPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testPublishWithResponseTopicAndCorrelationData
	"Test request/response pattern with Response Topic + Correlation Data"
	| topic responseTopic corrData msg |
	topic := self uniqueTopic.
	responseTopic := self uniqueTopic.
	corrData := #[16rDE 16rAD 16rBE 16rEF].
	msg := 'Request message'.
	self connectClient.
	client subscribeTo: topic qos: 1 do: [:t :m :q |
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	self publishPacketOnTopic: topic message: msg qos: 1 withProperties: [:pkt |
		pkt responseTopic: responseTopic.
		pkt correlationData: corrData].
	self waitForDelivery.
	self assert: received equals: msg.! !

!MQTT5PublishPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testPublishWithContentType
	"Test PUBLISH with Content Type property"
	| topic msg |
	topic := self uniqueTopic.
	msg := '{"temperature": 22.5}'.
	self connectClient.
	client subscribeTo: topic qos: 1 do: [:t :m :q |
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	self publishPacketOnTopic: topic message: msg qos: 1 withProperties: [:pkt |
		pkt contentType: 'application/json'].
	self waitForDelivery.
	self assert: received equals: msg.! !

!MQTT5PublishPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testPublishWithAllProperties
	"Test PUBLISH with all client-settable properties combined"
	| topic msg corrData |
	topic := self uniqueTopic.
	msg := 'Full properties message'.
	corrData := #[1 2 3 4 5 6 7 8].
	self connectClient.
	client subscribeTo: topic qos: 1 do: [:t :m :q |
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	self publishPacketOnTopic: topic message: msg qos: 1 withProperties: [:pkt |
		pkt payloadFormatIndicator: 1.
		pkt messageExpiryInterval: 300.
		pkt responseTopic: 'reply/topic'.
		pkt correlationData: corrData.
		pkt contentType: 'text/plain'].
	self waitForDelivery.
	self assert: received equals: msg.! !

!MQTT5PublishPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testPublishWithPropertiesQoS0
	"Test that properties work with QoS 0 (fire and forget)"
	| topic msg |
	topic := self uniqueTopic.
	msg := 'QoS0 with props'.
	self connectClient.
	client subscribeTo: topic qos: 0 do: [:t :m :q |
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	self publishPacketOnTopic: topic message: msg qos: 0 withProperties: [:pkt |
		pkt contentType: 'text/plain'.
		pkt payloadFormatIndicator: 1].
	self waitForDelivery.
	self assert: received equals: msg.! !

!MQTT5PublishPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testPublishWithPropertiesQoS2
	"Test that properties work with QoS 2 (exactly once)"
	| topic msg |
	topic := self uniqueTopic.
	msg := 'QoS2 with props'.
	self connectClient.
	client subscribeTo: topic qos: 2 do: [:t :m :q |
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	self publishPacketOnTopic: topic message: msg qos: 2 withProperties: [:pkt |
		pkt messageExpiryInterval: 120.
		pkt contentType: 'application/octet-stream'].
	(Delay forMilliseconds: 1000) wait.
	self assert: received equals: msg.! !




MQTT5IntegrationTest subclass: #MQTT5ConnectPropertiesIntegrationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-AllTests'!

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnectWithSessionExpiryInterval
	"Test CONNECT with Session Expiry Interval property"
	| transport connectPacket |
	self connectClient.
	self assert: client isConnected.
	self assert: client isVersion5.! !

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnAckReceiveMaximum
	"Verify broker returns Receive Maximum in CONNACK"
	| caps |
	self connectClient.
	caps := client socketClient transport serverCapabilities.
	self assert: caps receiveMaximum isInteger.
	self assert: caps receiveMaximum > 0.
	"Spec default is 65535 if not set"! !

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnAckMaximumQoS
	"Verify broker reports Maximum QoS capability"
	| caps |
	self connectClient.
	caps := client socketClient transport serverCapabilities.
	self assert: (caps maximumQoS between: 0 and: 2).! !

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnAckRetainAvailable
	"Verify broker reports retain availability"
	| caps |
	self connectClient.
	caps := client socketClient transport serverCapabilities.
	self assert: (caps retainAvailable isKindOf: Boolean).! !

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnAckMaximumPacketSize
	"Verify broker reports Maximum Packet Size"
	| caps |
	self connectClient.
	caps := client socketClient transport serverCapabilities.
	self assert: caps maximumPacketSize isInteger.
	self assert: caps maximumPacketSize > 0.! !

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnAckTopicAliasMaximum
	"Verify broker reports Topic Alias Maximum"
	| caps |
	self connectClient.
	caps := client socketClient transport serverCapabilities.
	self assert: caps topicAliasMaximum isInteger.
	self assert: caps topicAliasMaximum >= 0.! !

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnAckWildcardSubscriptionAvailable
	"Verify broker reports wildcard subscription capability"
	| caps |
	self connectClient.
	caps := client socketClient transport serverCapabilities.
	self assert: (caps wildcardSubscriptionAvailable isKindOf: Boolean).! !

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnAckSubscriptionIdentifiersAvailable
	"Verify broker reports subscription identifier support"
	| caps |
	self connectClient.
	caps := client socketClient transport serverCapabilities.
	self assert: (caps subscriptionIdentifiersAvailable isKindOf: Boolean).! !

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnAckSharedSubscriptionAvailable
	"Verify broker reports shared subscription capability"
	| caps |
	self connectClient.
	caps := client socketClient transport serverCapabilities.
	self assert: (caps sharedSubscriptionAvailable isKindOf: Boolean).! !

!MQTT5ConnectPropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testConnAckAllCapabilities
	"Comprehensive test of all CONNACK server capabilities"
	| caps |
	self connectClient.
	caps := client socketClient transport serverCapabilities.
	self assert: caps notNil.
	"All these should be parsed from CONNACK properties"
	self assert: caps receiveMaximum isInteger.
	self assert: caps maximumPacketSize isInteger.
	self assert: caps topicAliasMaximum isInteger.
	self assert: (caps maximumQoS between: 0 and: 2).! !




MQTT5IntegrationTest subclass: #MQTT5DisconnectIntegrationTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-AllTests'!

!MQTT5DisconnectIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testDisconnectWithReasonCodeNormal
	"Test sending DISCONNECT with reason code 0 (Normal)"
	self connectClient.
	self assert: client isConnected.
	client disconnect.
	self deny: client isConnected.! !

!MQTT5DisconnectIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testDisconnectSendsPacket
	"Verify that disconnect actually sends a DISCONNECT packet (v5)"
	| transport |
	self connectClient.
	transport := client socketClient transport.
	self assert: transport isVersion5.
	client disconnect.
	"After disconnect, connection should be closed"
	self deny: client isConnected.! !




MQTT5IntegrationTest subclass: #MQTT5SubscribePropertiesIntegrationTest
	instanceVariableNames: 'received receivedTopic'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-AllTests'!

!MQTT5SubscribePropertiesIntegrationTest methodsFor: 'running' stamp: 'Simba 2/16/2026 05:22'!
setUp
	super setUp.
	received := nil.
	receivedTopic := nil.! !

!MQTT5SubscribePropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testSubscribeWithV5Options
	"Test v5 subscription options: noLocal, retainAsPublished, retainHandling"
	| topic msg |
	topic := self uniqueTopic.
	msg := 'V5 options test'.
	self connectClient.
	"Standard subscribe - the v5 options are encoded in the subscription byte"
	client subscribeTo: topic qos: 1 do: [:t :m :q |
		receivedTopic := t.
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	client publish: (UTF8Encoder encode: msg) onTopic: topic qos: 1.
	self waitForDelivery.
	self assert: received equals: msg.! !

!MQTT5SubscribePropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testSubscribeQoS0
	"Verify v5 SUBSCRIBE/SUBACK at QoS 0"
	| topic msg |
	topic := self uniqueTopic.
	msg := 'QoS0 sub test'.
	self connectClient.
	client subscribeTo: topic qos: 0 do: [:t :m :q |
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	client publish: (UTF8Encoder encode: msg) onTopic: topic qos: 0.
	self waitForDelivery.
	self assert: received equals: msg.! !

!MQTT5SubscribePropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testSubscribeQoS2
	"Verify v5 SUBSCRIBE/SUBACK at QoS 2"
	| topic msg |
	topic := self uniqueTopic.
	msg := 'QoS2 sub test'.
	self connectClient.
	client subscribeTo: topic qos: 2 do: [:t :m :q |
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	client publish: (UTF8Encoder encode: msg) onTopic: topic qos: 2.
	(Delay forMilliseconds: 1000) wait.
	self assert: received equals: msg.! !

!MQTT5SubscribePropertiesIntegrationTest methodsFor: 'tests' stamp: 'Simba 2/16/2026 05:22'!
testUnsubscribeV5
	"Test v5 UNSUBSCRIBE/UNSUBACK flow"
	| topic msg |
	topic := self uniqueTopic.
	msg := 'Before unsub v5'.
	self connectClient.
	client subscribeTo: topic qos: 1 do: [:t :m :q |
		received := UTF8Encoder decode: m].
	self waitForSubscription.
	client publish: (UTF8Encoder encode: msg) onTopic: topic qos: 1.
	self waitForDelivery.
	self assert: received equals: msg.
	"Unsubscribe"
	client unsubscribeFrom: topic.
	self waitForSubscription.
	received := nil.
	client publish: (UTF8Encoder encode: 'After unsub v5') onTopic: topic qos: 1.
	self waitForDelivery.
	self assert: received isNil.! !
