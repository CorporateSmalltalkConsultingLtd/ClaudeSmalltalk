'From Cuis 6.0 [latest update: #6411] on 18 January 2026 at 12:00:00 pm'!
'Description MQTT-Cuis Test Suite - Unit tests for MQTT packet encoding/decoding'!

!provides: 'MQTT-Cuis-Tests' 1 0!
!requires: 'MQTT-Cuis' 1 0 nil!

SystemOrganization addCategory: #'MQTT-Cuis-Tests'!


!classDefinition: #MockSocketStream category: #'MQTT-Cuis-Tests'!
Object subclass: #MockSocketStream
	instanceVariableNames: 'readBuffer writeBuffer position'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-Tests'!

!MockSocketStream commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
MockSocketStream simulates a SocketStream for testing MQTT packet encoding/decoding.
It captures written bytes and allows setting up bytes to be read.!


!classDefinition: #MQTTPacketTest category: #'MQTT-Cuis-Tests'!
TestCase subclass: #MQTTPacketTest
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-Tests'!

!MQTTPacketTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
MQTTPacketTest tests basic packet encoding/decoding functionality.!


!classDefinition: #MQTTConnectPacketTest category: #'MQTT-Cuis-Tests'!
TestCase subclass: #MQTTConnectPacketTest
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-Tests'!

!MQTTConnectPacketTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
MQTTConnectPacketTest tests CONNECT and CONNACK packet encoding/decoding.!


!classDefinition: #MQTTPublishPacketTest category: #'MQTT-Cuis-Tests'!
TestCase subclass: #MQTTPublishPacketTest
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-Tests'!

!MQTTPublishPacketTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
MQTTPublishPacketTest tests PUBLISH packet encoding/decoding with different QoS levels.!


!classDefinition: #MQTTSubscribePacketTest category: #'MQTT-Cuis-Tests'!
TestCase subclass: #MQTTSubscribePacketTest
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-Tests'!

!MQTTSubscribePacketTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
MQTTSubscribePacketTest tests SUBSCRIBE and SUBACK packet encoding/decoding.!


!classDefinition: #MQTTUnsubscribePacketTest category: #'MQTT-Cuis-Tests'!
TestCase subclass: #MQTTUnsubscribePacketTest
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-Tests'!

!MQTTUnsubscribePacketTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
MQTTUnsubscribePacketTest tests UNSUBSCRIBE and UNSUBACK packet encoding/decoding.!


!classDefinition: #MQTTTransportLayerTest category: #'MQTT-Cuis-Tests'!
TestCase subclass: #MQTTTransportLayerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis-Tests'!

!MQTTTransportLayerTest commentStamp: 'JMM 1/18/2026 12:00' prior: 0!
MQTTTransportLayerTest tests transport layer functionality like message ID generation.!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MockSocketStream methods"!

!MockSocketStream methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	super initialize.
	readBuffer := OrderedCollection new.
	writeBuffer := OrderedCollection new.
	position := 1! !

!MockSocketStream methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
writtenBytes
	"Return all bytes written to this stream"
	^ writeBuffer asArray! !

!MockSocketStream methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
writtenByteArray
	"Return all bytes written as a ByteArray"
	^ writeBuffer asByteArray! !

!MockSocketStream methodsFor: 'setup' stamp: 'JMM 1/18/2026 12:00'!
nextPutAllToRead: aByteArray
	"Set up bytes to be read from this stream"
	aByteArray do: [:byte | readBuffer add: byte].
	position := 1! !

!MockSocketStream methodsFor: 'stream out' stamp: 'JMM 1/18/2026 12:00'!
nextPut: aByte
	writeBuffer add: aByte.
	^ aByte! !

!MockSocketStream methodsFor: 'stream out' stamp: 'JMM 1/18/2026 12:00'!
nextPutAll: aCollection
	aCollection do: [:byte | writeBuffer add: byte].
	^ aCollection! !

!MockSocketStream methodsFor: 'stream out' stamp: 'JMM 1/18/2026 12:00'!
next: n putAll: aCollection startingAt: startIndex
	"Write n bytes from aCollection starting at startIndex"
	startIndex to: startIndex + n - 1 do: [:i |
		writeBuffer add: (aCollection at: i)].
	^ n! !

!MockSocketStream methodsFor: 'stream out' stamp: 'JMM 1/18/2026 12:00'!
flush
	"No-op for testing"! !

!MockSocketStream methodsFor: 'stream in' stamp: 'JMM 1/18/2026 12:00'!
next
	| byte |
	position > readBuffer size ifTrue: [^ nil].
	byte := readBuffer at: position.
	position := position + 1.
	^ byte! !

!MockSocketStream methodsFor: 'stream in' stamp: 'JMM 1/18/2026 12:00'!
next: count
	| result |
	result := ByteArray new: count.
	1 to: count do: [:i | result at: i put: self next].
	^ result! !

!MockSocketStream methodsFor: 'stream in' stamp: 'JMM 1/18/2026 12:00'!
peek
	position > readBuffer size ifTrue: [^ nil].
	^ readBuffer at: position! !

!MockSocketStream methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
atEnd
	^ position > readBuffer size! !

!MockSocketStream methodsFor: 'configuration' stamp: 'JMM 1/18/2026 12:00'!
binary
	"No-op for testing"! !

!MockSocketStream methodsFor: 'configuration' stamp: 'JMM 1/18/2026 12:00'!
timeout: seconds
	"No-op for testing"! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketTest methods"!

!MQTTPacketTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
setUp
	super setUp.
	stream := MockSocketStream new! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEncode16BitInteger
	"Test encoding a 16-bit integer"
	| packet |
	packet := MQTTPacket new.
	packet encode16BitInteger: 16r1234 on: stream.
	self assert: stream writtenBytes equals: #(16r12 16r34)! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testDecode16BitInteger
	"Test decoding a 16-bit integer"
	| packet result |
	stream nextPutAllToRead: #[16r12 16r34].
	packet := MQTTPacket new.
	result := packet decode16BitIntegerFrom: stream.
	self assert: result equals: 16r1234! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEncodeString
	"Test encoding a string with length prefix"
	| packet |
	packet := MQTTPacket new.
	packet encodeString: 'test' on: stream.
	self assert: stream writtenBytes equals: #(0 4 116 101 115 116)! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testDecodeString
	"Test decoding a length-prefixed string"
	| packet result |
	stream nextPutAllToRead: #[0 4 116 101 115 116].
	packet := MQTTPacket new.
	result := packet decodeStringFrom: stream.
	self assert: result equals: 'test'! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEncodeVariableLength127
	"Test encoding variable length for value <= 127"
	| packet |
	packet := MQTTPacket new.
	packet encodeVariableLength: 64 on: stream.
	self assert: stream writtenBytes equals: #(64)! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEncodeVariableLength128
	"Test encoding variable length for value = 128"
	| packet |
	packet := MQTTPacket new.
	packet encodeVariableLength: 128 on: stream.
	self assert: stream writtenBytes equals: #(16r80 16r01)! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testEncodeVariableLengthLarge
	"Test encoding variable length for larger value"
	| packet |
	packet := MQTTPacket new.
	packet encodeVariableLength: 16384 on: stream.
	self assert: stream writtenBytes equals: #(16r80 16r80 16r01)! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPacketTypeConnect
	"Test CONNECT packet type"
	| packet |
	packet := MQTTPacketConnect new.
	self assert: packet packetType equals: 1! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPacketTypeConnAck
	"Test CONNACK packet type"
	| packet |
	packet := MQTTPacketConnAck new.
	self assert: packet packetType equals: 2! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPacketTypePublish
	"Test PUBLISH packet type"
	| packet |
	packet := MQTTPacketPublish new.
	self assert: packet packetType equals: 3! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPacketTypeSubscribe
	"Test SUBSCRIBE packet type"
	| packet |
	packet := MQTTPacketSubscribe new.
	self assert: packet packetType equals: 8! !

!MQTTPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPacketTypeUnsubscribe
	"Test UNSUBSCRIBE packet type"
	| packet |
	packet := MQTTPacketUnsubscribe new.
	self assert: packet packetType equals: 10! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTConnectPacketTest methods"!

!MQTTConnectPacketTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
setUp
	super setUp.
	stream := MockSocketStream new! !

!MQTTConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnectPacketFixedHeader
	"Test CONNECT packet fixed header (type 1, flags 0)"
	| packet |
	packet := MQTTPacketConnect new.
	self assert: packet fixedHeader equals: 16r10! !

!MQTTConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnectPacketProtocolName
	"Test CONNECT packet includes 'MQTT' protocol name"
	| packet |
	packet := MQTTPacketConnect new.
	packet clientID: 'testClient'.
	packet encodeVariableHeaderOn: stream.
	"Should start with length-prefixed 'MQTT'"
	self assert: (stream writtenBytes copyFrom: 1 to: 6) equals: #(0 4 77 81 84 84)! !

!MQTTConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnectPacketProtocolLevel
	"Test CONNECT packet includes protocol level 4 (MQTT 3.1.1)"
	| packet bytes |
	packet := MQTTPacketConnect new.
	packet clientID: 'testClient'.
	packet encodeVariableHeaderOn: stream.
	bytes := stream writtenBytes.
	"Byte 7 should be protocol level 4"
	self assert: (bytes at: 7) equals: 4! !

!MQTTConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnectPacketCleanSession
	"Test CONNECT packet with clean session flag"
	| packet bytes |
	packet := MQTTPacketConnect new.
	packet clientID: 'testClient'.
	packet cleanSessionFlag: true.
	packet encodeVariableHeaderOn: stream.
	bytes := stream writtenBytes.
	"Byte 8 is connect flags, bit 1 is clean session"
	self assert: ((bytes at: 8) allMask: 2)! !

!MQTTConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnectPacketKeepAlive
	"Test CONNECT packet keepalive encoding"
	| packet bytes |
	packet := MQTTPacketConnect new.
	packet clientID: 'testClient'.
	packet keepAliveTime: 60.
	packet encodeVariableHeaderOn: stream.
	bytes := stream writtenBytes.
	"Bytes 9-10 are keep alive (60 = 0x003C)"
	self assert: (bytes at: 9) equals: 0.
	self assert: (bytes at: 10) equals: 60! !

!MQTTConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnectPacketWithCredentials
	"Test CONNECT packet with username and password"
	| packet bytes |
	packet := MQTTPacketConnect new.
	packet clientID: 'testClient'.
	packet user: 'testUser'.
	packet password: 'testPass'.
	packet encodeVariableHeaderOn: stream.
	bytes := stream writtenBytes.
	"Connect flags should have user flag (bit 7) and password flag (bit 6) set"
	self assert: ((bytes at: 8) allMask: 16r80).
	self assert: ((bytes at: 8) allMask: 16r40)! !

!MQTTConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnAckPacketDecode
	"Test decoding CONNACK packet"
	| packet |
	"CONNACK: type 2, length 2, session present 0, return code 0"
	stream nextPutAllToRead: #[16r20 2 0 0].
	packet := MQTTPacket readFrom: stream.
	self assert: packet class equals: MQTTPacketConnAck.
	self assert: packet returnCode equals: 0! !

!MQTTConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnAckPacketDecodeWithError
	"Test decoding CONNACK packet with error code"
	| packet |
	"CONNACK: type 2, length 2, session present 0, return code 4 (bad credentials)"
	stream nextPutAllToRead: #[16r20 2 0 4].
	packet := MQTTPacket readFrom: stream.
	self assert: packet class equals: MQTTPacketConnAck.
	self assert: packet returnCode equals: 4! !

!MQTTConnectPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testConnAckPacketEncode
	"Test encoding CONNACK packet"
	| packet |
	packet := MQTTPacketConnAck new.
	packet sessionPresent: false.
	packet returnCode: 0.
	packet encodeOn: stream.
	"Should be: type/flags (0x20), length (2), session present (0), return code (0)"
	self assert: stream writtenBytes equals: #(16r20 2 0 0)! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPublishPacketTest methods"!

!MQTTPublishPacketTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
setUp
	super setUp.
	stream := MockSocketStream new! !

!MQTTPublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPublishPacketQos0
	"Test PUBLISH packet with QoS 0 (no message ID)"
	| packet bytes |
	packet := MQTTPacketPublish new.
	packet topic: 'test/topic'.
	packet message: 'hello' asByteArray.
	packet qos: 0.
	packet encodeOn: stream.
	bytes := stream writtenBytes.
	"Fixed header for QoS 0: type 3, flags 0 = 0x30"
	self assert: bytes first equals: 16r30! !

!MQTTPublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPublishPacketQos1
	"Test PUBLISH packet with QoS 1 (has message ID)"
	| packet bytes |
	packet := MQTTPacketPublish new.
	packet topic: 'test/topic'.
	packet message: 'hello' asByteArray.
	packet qos: 1.
	packet packetID: 1.
	packet encodeOn: stream.
	bytes := stream writtenBytes.
	"Fixed header for QoS 1: type 3, flags 2 = 0x32"
	self assert: bytes first equals: 16r32! !

!MQTTPublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPublishPacketQos2
	"Test PUBLISH packet with QoS 2"
	| packet bytes |
	packet := MQTTPacketPublish new.
	packet topic: 'test/topic'.
	packet message: 'hello' asByteArray.
	packet qos: 2.
	packet packetID: 1.
	packet encodeOn: stream.
	bytes := stream writtenBytes.
	"Fixed header for QoS 2: type 3, flags 4 = 0x34"
	self assert: bytes first equals: 16r34! !

!MQTTPublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPublishPacketWithRetain
	"Test PUBLISH packet with retain flag"
	| packet bytes |
	packet := MQTTPacketPublish new.
	packet topic: 'test/topic'.
	packet message: 'hello' asByteArray.
	packet qos: 0.
	packet retain: true.
	packet encodeOn: stream.
	bytes := stream writtenBytes.
	"Fixed header for QoS 0 with retain: type 3, flags 1 = 0x31"
	self assert: bytes first equals: 16r31! !

!MQTTPublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPublishPacketDecode
	"Test decoding PUBLISH packet"
	| packet topicBytes msgBytes allBytes |
	"Build a QoS 0 PUBLISH: topic 'a/b', message 'hi'"
	topicBytes := #[0 3 97 47 98].  "length 3, 'a/b'"
	msgBytes := #[104 105].  "hi"
	allBytes := ByteArray new: 2 + topicBytes size + msgBytes size.
	allBytes at: 1 put: 16r30.  "PUBLISH QoS 0"
	allBytes at: 2 put: topicBytes size + msgBytes size.
	allBytes replaceFrom: 3 to: 2 + topicBytes size with: topicBytes.
	allBytes replaceFrom: 3 + topicBytes size to: allBytes size with: msgBytes.
	stream nextPutAllToRead: allBytes.
	packet := MQTTPacket readFrom: stream.
	self assert: packet class equals: MQTTPacketPublish.
	self assert: packet topic equals: 'a/b'.
	self assert: packet message asString equals: 'hi'! !

!MQTTPublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPubAckPacketEncode
	"Test encoding PUBACK packet"
	| packet |
	packet := MQTTPacketPubAck new.
	packet packetID: 1234.
	packet encodeOn: stream.
	"PUBACK: type 4, length 2, msgID high, msgID low"
	self assert: stream writtenBytes equals: #(16r40 2 4 210)! !

!MQTTPublishPacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPubAckPacketDecode
	"Test decoding PUBACK packet"
	| packet |
	stream nextPutAllToRead: #[16r40 2 4 210].
	packet := MQTTPacket readFrom: stream.
	self assert: packet class equals: MQTTPacketPubAck.
	self assert: packet packetID equals: 1234! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTSubscribePacketTest methods"!

!MQTTSubscribePacketTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
setUp
	super setUp.
	stream := MockSocketStream new! !

!MQTTSubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testSubscribePacketFixedHeader
	"Test SUBSCRIBE packet fixed header (type 8, flags 2)"
	| packet |
	packet := MQTTPacketSubscribe new.
	self assert: packet fixedHeader equals: 16r82! !

!MQTTSubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testSubscribePacketEncode
	"Test encoding SUBSCRIBE packet"
	| packet bytes |
	packet := MQTTPacketSubscribe new.
	packet packetID: 1.
	packet addTopic: 'test/topic' qos: 1.
	packet encodeOn: stream.
	bytes := stream writtenBytes.
	"Fixed header should be 0x82"
	self assert: bytes first equals: 16r82! !

!MQTTSubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testSubscribePacketWithMultipleTopics
	"Test SUBSCRIBE packet with multiple topics"
	| packet |
	packet := MQTTPacketSubscribe new.
	packet packetID: 1.
	packet addTopic: 'topic1' qos: 0.
	packet addTopic: 'topic2' qos: 1.
	packet addTopic: 'topic3' qos: 2.
	self assert: packet topics size equals: 3! !

!MQTTSubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testSubAckPacketDecode
	"Test decoding SUBACK packet"
	| packet |
	"SUBACK: type 9, length 3, msgID 0 1, return code 1"
	stream nextPutAllToRead: #[16r90 3 0 1 1].
	packet := MQTTPacket readFrom: stream.
	self assert: packet class equals: MQTTPacketSubAck.
	self assert: packet packetID equals: 1! !

!MQTTSubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testSubAckPacketEncode
	"Test encoding SUBACK packet"
	| packet |
	packet := MQTTPacketSubAck new.
	packet packetID: 1.
	packet returnCodes: #(0 1 2).
	packet encodeOn: stream.
	"SUBACK: type 9, length 5, msgID 0 1, return codes 0 1 2"
	self assert: stream writtenBytes equals: #(16r90 5 0 1 0 1 2)! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTUnsubscribePacketTest methods"!

!MQTTUnsubscribePacketTest methodsFor: 'running' stamp: 'JMM 1/18/2026 12:00'!
setUp
	super setUp.
	stream := MockSocketStream new! !

!MQTTUnsubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testUnsubscribePacketFixedHeader
	"Test UNSUBSCRIBE packet fixed header (type 10, flags 2)"
	| packet |
	packet := MQTTPacketUnsubscribe new.
	self assert: packet fixedHeader equals: 16rA2! !

!MQTTUnsubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testUnsubscribePacketEncode
	"Test encoding UNSUBSCRIBE packet"
	| packet bytes |
	packet := MQTTPacketUnsubscribe new.
	packet packetID: 1.
	packet addTopic: 'test/topic'.
	packet encodeOn: stream.
	bytes := stream writtenBytes.
	"Fixed header should be 0xA2"
	self assert: bytes first equals: 16rA2! !

!MQTTUnsubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testUnsubscribePacketWithMultipleTopics
	"Test UNSUBSCRIBE packet with multiple topics"
	| packet |
	packet := MQTTPacketUnsubscribe new.
	packet packetID: 1.
	packet addTopic: 'topic1'.
	packet addTopic: 'topic2'.
	self assert: packet topics size equals: 2! !

!MQTTUnsubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testUnsubAckPacketDecode
	"Test decoding UNSUBACK packet"
	| packet |
	"UNSUBACK: type 11, length 2, msgID 0 1"
	stream nextPutAllToRead: #[16rB0 2 0 1].
	packet := MQTTPacket readFrom: stream.
	self assert: packet class equals: MQTTPacketUnsubAck.
	self assert: packet packetID equals: 1! !

!MQTTUnsubscribePacketTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testUnsubAckPacketEncode
	"Test encoding UNSUBACK packet"
	| packet |
	packet := MQTTPacketUnsubAck new.
	packet packetID: 1.
	packet encodeOn: stream.
	"UNSUBACK: type 11, length 2, msgID 0 1"
	self assert: stream writtenBytes equals: #(16rB0 2 0 1)! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTTransportLayerTest methods"!

!MQTTTransportLayerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testClientIDGeneration
	"Test that client ID is generated"
	| clientID |
	clientID := MQTTTransportLayerClient clientID.
	self assert: clientID isString.
	self assert: clientID notEmpty.
	self assert: (clientID beginsWith: 'Cuis-')! !

!MQTTTransportLayerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testPingPackets
	"Test PINGREQ and PINGRESP packet types"
	| pingReq pingResp |
	pingReq := MQTTPacketPingReq new.
	pingResp := MQTTPacketPingResp new.
	self assert: pingReq packetType equals: 12.
	self assert: pingResp packetType equals: 13! !

!MQTTTransportLayerTest methodsFor: 'tests' stamp: 'JMM 1/18/2026 12:00'!
testDisconnectPacket
	"Test DISCONNECT packet type"
	| packet |
	packet := MQTTPacketDisconnect new.
	self assert: packet packetType equals: 14! !

