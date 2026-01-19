'From Cuis 6.0 [latest update: #6411] on 18 January 2026 at 12:00:00 pm'!
'Description MQTT Client Library for Cuis Smalltalk - Adapted from Pharo MQTT library by Tim Rowledge and John M McIntosh'!

!provides: 'MQTT-Cuis' 1 1!
!requires: 'Network-Kernel' 1 0 nil!

SystemOrganization addCategory: #'MQTT-Cuis'!

!classDefinition: #MQTTBadHeaderException category: #'MQTT-Cuis'!
Error subclass: #MQTTBadHeaderException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTBadPacketTypeException category: #'MQTT-Cuis'!
Error subclass: #MQTTBadPacketTypeException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTBadTopicException category: #'MQTT-Cuis'!
Error subclass: #MQTTBadTopicException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTBrokerLookupFailure category: #'MQTT-Cuis'!
Error subclass: #MQTTBrokerLookupFailure
	instanceVariableNames: 'brokerName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTCONNECTException category: #'MQTT-Cuis'!
Error subclass: #MQTTCONNECTException
	instanceVariableNames: 'connectReturnCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTConnectionException category: #'MQTT-Cuis'!
Error subclass: #MQTTConnectionException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTEmptyStreamErrorException category: #'MQTT-Cuis'!
Error subclass: #MQTTEmptyStreamErrorException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #UTF8Encoder category: #'MQTT-Cuis'!
Object subclass: #UTF8Encoder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTWriteStream category: #'MQTT-Cuis'!
WriteStream subclass: #MQTTWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTStatistics category: #'MQTT-Cuis'!
Object subclass: #MQTTStatistics
	instanceVariableNames: 'pin pout poutMaxSize poutMinSize pinMaxSize pinMinSize pinBytes poutBytes pinTypeCount poutTypeCount errorTypes connections disconnect connected connectionTypes servers subscriptions timeOut killSocket sentPackets queuedPackets unsubscribe badConnections missingType resendType'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTSubscription category: #'MQTT-Cuis'!
Object subclass: #MQTTSubscription
	instanceVariableNames: 'topic actionBlock qos retainFlag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketAndPendingJobPair category: #'MQTT-Cuis'!
Object subclass: #MQTTPacketAndPendingJobPair
	instanceVariableNames: 'packet pendingJob'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPendingJob category: #'MQTT-Cuis'!
Object subclass: #MQTTPendingJob
	instanceVariableNames: 'originalPacket sendTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPendingPingJob category: #'MQTT-Cuis'!
MQTTPendingJob subclass: #MQTTPendingPingJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPendingPubAckJob category: #'MQTT-Cuis'!
MQTTPendingJob subclass: #MQTTPendingPubAckJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPendingPubRecJob category: #'MQTT-Cuis'!
MQTTPendingJob subclass: #MQTTPendingPubRecJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPendingPubRelJob category: #'MQTT-Cuis'!
MQTTPendingJob subclass: #MQTTPendingPubRelJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPendingPubCompJob category: #'MQTT-Cuis'!
MQTTPendingJob subclass: #MQTTPendingPubCompJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPendingSubAckJob category: #'MQTT-Cuis'!
MQTTPendingJob subclass: #MQTTPendingSubAckJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPendingUnsubAckJob category: #'MQTT-Cuis'!
MQTTPendingJob subclass: #MQTTPendingUnsubAckJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacket category: #'MQTT-Cuis'!
Object subclass: #MQTTPacket
	instanceVariableNames: ''
	classVariableNames: 'MQTTProtocolLevel311 PacketTypes'
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketDisconnect category: #'MQTT-Cuis'!
MQTTPacket subclass: #MQTTPacketDisconnect
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketPingReq category: #'MQTT-Cuis'!
MQTTPacket subclass: #MQTTPacketPingReq
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketPingResp category: #'MQTT-Cuis'!
MQTTPacket subclass: #MQTTPacketPingResp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketVariableHeaded category: #'MQTT-Cuis'!
MQTTPacket subclass: #MQTTPacketVariableHeaded
	instanceVariableNames: 'remainingLength msgID'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketConnAck category: #'MQTT-Cuis'!
MQTTPacketVariableHeaded subclass: #MQTTPacketConnAck
	instanceVariableNames: 'sessionPresent byte1 byte2'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketPubAck category: #'MQTT-Cuis'!
MQTTPacketVariableHeaded subclass: #MQTTPacketPubAck
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketPubRec category: #'MQTT-Cuis'!
MQTTPacketVariableHeaded subclass: #MQTTPacketPubRec
	instanceVariableNames: 'originalPacket'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketPubRel category: #'MQTT-Cuis'!
MQTTPacketVariableHeaded subclass: #MQTTPacketPubRel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketPubComp category: #'MQTT-Cuis'!
MQTTPacketVariableHeaded subclass: #MQTTPacketPubComp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketUnsubAck category: #'MQTT-Cuis'!
MQTTPacketVariableHeaded subclass: #MQTTPacketUnsubAck
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketVariableHeadedWithPayload category: #'MQTT-Cuis'!
MQTTPacketVariableHeaded subclass: #MQTTPacketVariableHeadedWithPayload
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketConnect category: #'MQTT-Cuis'!
MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketConnect
	instanceVariableNames: 'user password keepAliveTime willMessage willTopic willQos willFlag willRetainFlag cleanSessionFlag clientID returnCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketPublish category: #'MQTT-Cuis'!
MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketPublish
	instanceVariableNames: 'topic message duplicate qos retain'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketSubscribe category: #'MQTT-Cuis'!
MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketSubscribe
	instanceVariableNames: 'payloadDict'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketSubAck category: #'MQTT-Cuis'!
MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketSubAck
	instanceVariableNames: 'returnCodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTPacketUnsubscribe category: #'MQTT-Cuis'!
MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketUnsubscribe
	instanceVariableNames: 'topics'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTTransportLayer category: #'MQTT-Cuis'!
Object subclass: #MQTTTransportLayer
	instanceVariableNames: 'sockStrm abort incomingPacketProcess packetInFlightQueue outgoingPacketQueue outgoingPacketProcess retryTime pendingJobsMutex abortMutex retryProcess lastMID pendingJobs currentSubscriptions readWaitTime statisticsMutex statistics'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTTransportLayerClient category: #'MQTT-Cuis'!
MQTTTransportLayer subclass: #MQTTTransportLayerClient
	instanceVariableNames: 'pingOK keepAliveTime clientID socketClient'
	classVariableNames: 'ClientID'
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTSocketClient category: #'MQTT-Cuis'!
Object subclass: #MQTTSocketClient
	instanceVariableNames: 'transport cleanSessionFlag willTopic willMessage willRetainFlag willQos userNameString passwordString keepAliveTime socket interface pendingSocket pendingClientID'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!

!classDefinition: #MQTTClientInterface category: #'MQTT-Cuis'!
Object subclass: #MQTTClientInterface
	instanceVariableNames: 'socketClient hostName keepAliveTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT-Cuis'!


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"UTF8Encoder class methods"!

!UTF8Encoder class methodsFor: 'encoding' stamp: 'JMM 1/19/2026 12:00'!
decode: aByteArray
	"Decode UTF-8 bytes to a String or UnicodeString as appropriate. Uses native Cuis support."
	aByteArray isString ifTrue: [^ aByteArray].
	^ CharacterSequence fromUtf8Bytes: aByteArray! !

!UTF8Encoder class methodsFor: 'encoding' stamp: 'JMM 1/19/2026 12:00'!
encode: aString
	"Encode a String to UTF-8 bytes. Uses native Cuis support."
	^ aString asUtf8Bytes! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTWriteStream methods"!

!MQTTWriteStream methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	super initialize.
	"Binary mode for MQTT packets"! !

!MQTTWriteStream class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
on: aCollection
	^ super on: aCollection! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTBrokerLookupFailure methods"!

!MQTTBrokerLookupFailure class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
brokerName: aString
	^ self new brokerName: aString! !

!MQTTBrokerLookupFailure methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
brokerName
	^ brokerName! !

!MQTTBrokerLookupFailure methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
brokerName: aString
	brokerName := aString! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTCONNECTException methods"!

!MQTTCONNECTException class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
connectReturnCode: aByte
	^ (self new connectReturnCode: aByte) signal! !

!MQTTCONNECTException methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
connectReturnCode
	^ connectReturnCode! !

!MQTTCONNECTException methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
connectReturnCode: aValue
	connectReturnCode := aValue! !

!MQTTCONNECTException methodsFor: 'printing' stamp: 'JMM 1/18/2026 12:00'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' returnCode: '.
	connectReturnCode printString printOn: aStream! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTStatistics methods"!

!MQTTStatistics methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	pin := 0.
	pout := 0.
	poutMaxSize := 0.
	poutMinSize := 1000000.
	poutBytes := 0.
	poutTypeCount := Bag new.
	pinMaxSize := 0.
	pinMinSize := 1000000.
	pinBytes := 0.
	pinTypeCount := Bag new.
	errorTypes := Bag new.
	connections := 0.
	disconnect := 0.
	connected := Set new.
	connectionTypes := Bag new.
	servers := 0.
	subscriptions := Bag new.
	timeOut := 0.
	killSocket := 0.
	sentPackets := 0.
	queuedPackets := 0.
	unsubscribe := 0.
	badConnections := 0.
	missingType := Bag new.
	resendType := Bag new! !

!MQTTStatistics methodsFor: 'counting' stamp: 'JMM 1/18/2026 12:00'!
badConnections
	badConnections := badConnections + 1! !

!MQTTStatistics methodsFor: 'counting' stamp: 'JMM 1/18/2026 12:00'!
disconnect
	disconnect := disconnect + 1! !

!MQTTStatistics methodsFor: 'counting' stamp: 'JMM 1/18/2026 12:00'!
errorTypes: aMessage
	errorTypes add: aMessage! !

!MQTTStatistics methodsFor: 'counting' stamp: 'JMM 1/18/2026 12:00'!
missingType: aType
	missingType add: aType! !

!MQTTStatistics methodsFor: 'counting' stamp: 'JMM 1/18/2026 12:00'!
resendType: aType
	resendType add: aType! !

!MQTTStatistics methodsFor: 'counting' stamp: 'JMM 1/18/2026 12:00'!
packetIn: aPacket
	| pType |
	pin := pin + 1.
	pType := aPacket class.
	pinTypeCount add: pType asString asSymbol.
	pType = MQTTPacketPublish ifTrue: [
		| sz |
		sz := aPacket message size.
		pinBytes := pinBytes + sz.
		pinMaxSize < sz ifTrue: [pinMaxSize := sz].
		pinMinSize > sz ifTrue: [pinMinSize := sz]]! !

!MQTTStatistics methodsFor: 'counting' stamp: 'JMM 1/18/2026 12:00'!
packetOut: aPacket
	| pType thePacket |
	pout := pout + 1.
	pType := aPacket class.
	thePacket := aPacket.
	pType = MQTTPacketAndPendingJobPair ifTrue: [
		pType := aPacket packet class.
		thePacket := aPacket packet].
	poutTypeCount add: pType asString asSymbol.
	pType = MQTTPacketPublish ifTrue: [
		| sz |
		sz := thePacket message size.
		poutBytes := poutBytes + sz.
		poutMaxSize < sz ifTrue: [poutMaxSize := sz].
		poutMinSize > sz ifTrue: [poutMinSize := sz]]! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTSubscription methods"!

!MQTTSubscription class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
for: aTopic qos: qos do: aBlock
	^ super new for: aTopic qos: qos do: aBlock! !

!MQTTSubscription methodsFor: 'comparing' stamp: 'JMM 1/18/2026 12:00'!
= aSubscription
	(aSubscription class = self class) ifFalse: [^ false].
	^ topic = aSubscription topic! !

!MQTTSubscription methodsFor: 'comparing' stamp: 'JMM 1/18/2026 12:00'!
hash
	^ topic hash! !

!MQTTSubscription methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
for: aTopic qos: aQos do: aBlock
	| strm priorChar currentChar nextChar count |
	topic := aTopic.
	actionBlock := aBlock.
	qos := aQos.
	priorChar := nil.
	count := 0.
	retainFlag := true.
	strm := aTopic readStream.
	[currentChar := [strm next] on: Error do: [:e | nil]] whileNotNil: [
		nextChar := strm peek.
		currentChar = $+ ifTrue: [
			((priorChar notNil and: [priorChar ~= $/]) or: [nextChar notNil and: [nextChar ~= $/]])
				ifTrue: [^ nil]]
		ifFalse: [
			currentChar = $# ifTrue: [
				((priorChar notNil and: [priorChar ~= $/]) or: [nextChar notNil])
					ifTrue: [^ nil]]
			ifFalse: [
				currentChar = Character null ifTrue: [^ nil]]].
		count := count + 1.
		priorChar := currentChar].
	count > 65535 ifTrue: [^ nil]! !

!MQTTSubscription methodsFor: 'handling' stamp: 'JMM 1/18/2026 12:00'!
handlePacket: aPublishPacket
	aPublishPacket matchesSubscription: self ifTrue: actionBlock! !

!MQTTSubscription methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
topic
	^ topic! !

!MQTTSubscription methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
qos
	^ qos! !

!MQTTSubscription methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
retainFlag
	^ retainFlag! !

!MQTTSubscription methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
retainFlag: aValue
	retainFlag := aValue! !

!MQTTSubscription methodsFor: 'matching' stamp: 'JMM 1/18/2026 12:00'!
topicMatches: candidateString
	| result multilevel topicLen csLen topicPos csPos |
	result := true.
	multilevel := false.
	topicLen := topic size.
	csLen := candidateString size.
	(topicLen > 0 and: [csLen > 0]) ifTrue: [
		(topic first = $$ xor: candidateString first = $$) ifTrue: [^ false]].
	topicPos := csPos := 1.
	[topicPos <= topicLen and: [csPos <= csLen]] whileTrue: [
		(topic at: topicPos) = (candidateString at: csPos) ifTrue: [
			csPos = csLen ifTrue: [
				(topicPos = (topicLen - 2) and: [(topic last: 2) = '/#']) ifTrue: [^ true]].
			topicPos := topicPos + 1.
			csPos := csPos + 1.
			(csPos > csLen and: [topicPos > topicLen]) ifTrue: [^ true]
			ifFalse: [
				((csPos > csLen and: [topicPos = topicLen]) & (topic last = $+)) ifTrue: [^ true]]]
		ifFalse: [
			(topic at: topicPos) = $+ ifTrue: [
				topicPos := topicPos + 1.
				[csPos <= csLen and: [(candidateString at: csPos) ~= $/]] whileTrue: [csPos := csPos + 1].
				(csPos > csLen and: [topicPos > topicLen]) ifTrue: [^ true]]
			ifFalse: [
				(topic at: topicPos) = $# ifTrue: [
					multilevel := true.
					^ topicPos = topicLen]
				ifFalse: [^ false]]]].
	(multilevel not and: [csPos <= csLen or: [topicPos <= topicLen]]) ifTrue: [^ false].
	^ result! !

!MQTTSubscription methodsFor: 'printing' stamp: 'JMM 1/18/2026 12:00'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' t: '.
	topic asString printOn: aStream.
	aStream nextPutAll: ' r: '.
	retainFlag asString printOn: aStream.
	aStream nextPutAll: ' qos: '.
	qos asString printOn: aStream! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketAndPendingJobPair methods"!

!MQTTPacketAndPendingJobPair class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
packet: aPacket pendingJob: aPendingJob
	| instance |
	instance := self new.
	instance packet: aPacket.
	instance pendingJob: aPendingJob.
	^ instance! !

!MQTTPacketAndPendingJobPair methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeOn: sockStrm
	| ret |
	ret := self packet encodeOn: sockStrm.
	pendingJob ifNotNil: [pendingJob resetSendTime: 20].
	^ ret! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packet
	^ packet! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packet: aValue
	packet := aValue! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
pendingJob
	^ pendingJob! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
pendingJob: aValue
	pendingJob := aValue! !

!MQTTPacketAndPendingJobPair methodsFor: 'printing' stamp: 'JMM 1/18/2026 12:00'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' '.
	packet asString printOn: aStream.
	aStream nextPutAll: ' '.
	pendingJob asString printOn: aStream! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPendingJob methods"!

!MQTTPendingJob class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
for: anMQTTPacket
	^ self new originalPacket: anMQTTPacket! !

!MQTTPendingJob class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
forDoNotTouchDupFlag: anMQTTPacket
	^ self new originalPacketDoNotTouchDupFlag: anMQTTPacket! !

!MQTTPendingJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
completeFor: anMQTTClient
	anMQTTClient releasePendingJob: self! !

!MQTTPendingJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
debugLog: aString
	MQTTClientInterface debugLog: self packetID printString tag: ' PJ MsgID:' str2: aString! !

!MQTTPendingJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isPubAckID: msgID
	^ false! !

!MQTTPendingJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isPubCompID: msgID
	^ false! !

!MQTTPendingJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isPubRecID: msgID
	^ false! !

!MQTTPendingJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isPubRelID: msgID
	^ false! !

!MQTTPendingJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isSubAckID: msgID
	^ false! !

!MQTTPendingJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isUnsubAckID: msgID
	^ false! !

!MQTTPendingJob methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
originalPacket
	^ originalPacket! !

!MQTTPendingJob methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
originalPacket: mqttPacket
	originalPacket := mqttPacket.
	sendTime := Time localSecondClock + 60 * 60 * 23! !

!MQTTPendingJob methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
originalPacketDoNotTouchDupFlag: mqttPacket
	self originalPacket: mqttPacket! !

!MQTTPendingJob methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetID
	^ originalPacket packetID! !

!MQTTPendingJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
resendFor: anMQTTClient ifNeededAtTime: seconds
	seconds >= sendTime ifTrue: [
		sendTime := Time localSecondClock.
		self originalPacket: self originalPacket.
		anMQTTClient statPerform: #resendType: with: originalPacket class asString.
		anMQTTClient sendPacket: (MQTTPacketAndPendingJobPair packet: originalPacket pendingJob: self)]! !

!MQTTPendingJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
resetSendTime: aTime
	sendTime := Time localSecondClock + aTime! !

!MQTTPendingJob methodsFor: 'printing' stamp: 'JMM 1/18/2026 12:00'!
printOn: aStream
	super printOn: aStream.
	self packetID ifNotNil: [
		aStream nextPutAll: ' msgID: '.
		self packetID printOn: aStream]! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPendingPingJob methods"!

!MQTTPendingPingJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
completeFor: anMQTTClient
	"I do nothing here; I'm so very special"! !

!MQTTPendingPingJob methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	sendTime := Time localSecondClock! !

!MQTTPendingPingJob methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetID
	^ 16r10000! !

!MQTTPendingPingJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
resendFor: anMQTTClient ifNeededAtTime: seconds
	seconds >= sendTime ifTrue: [sendTime := anMQTTClient ping]! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPendingPubAckJob methods"!

!MQTTPendingPubAckJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isPubAckID: msgID
	^ self packetID = msgID! !

!MQTTPendingPubAckJob methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
originalPacket
	originalPacket prepareForResend.
	^ originalPacket! !

!MQTTPendingPubAckJob methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
originalPacketDoNotTouchDupFlag: mqttPacket
	super originalPacket: mqttPacket! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPendingPubRecJob methods"!

!MQTTPendingPubRecJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
completeFor: anMQTTClient
	anMQTTClient releasePendingJob: self! !

!MQTTPendingPubRecJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isPubRecID: msgID
	^ self packetID = msgID! !

!MQTTPendingPubRecJob methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
originalPacket
	originalPacket prepareForResend.
	^ originalPacket! !

!MQTTPendingPubRecJob methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
originalPacketDoNotTouchDupFlag: mqttPacket
	super originalPacket: mqttPacket! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPendingPubRelJob methods"!

!MQTTPendingPubRelJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isPubRelID: msgID
	^ self packetID = msgID! !

!MQTTPendingPubRelJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
resendFor: anMQTTClient ifNeededAtTime: seconds
	seconds >= sendTime ifTrue: [
		sendTime := Time localSecondClock.
		self debugLog: '*******************resendFor MQTTPacketPubRec for ' , originalPacket packetID asString.
		anMQTTClient statPerform: #resendType: with: MQTTPacketPubRec asString.
		anMQTTClient sendPacket: (MQTTPacketPubRec new packetID: originalPacket packetID)]! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPendingPubCompJob methods"!

!MQTTPendingPubCompJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isPubCompID: msgID
	^ self packetID = msgID! !

!MQTTPendingPubCompJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
resendFor: anMQTTClient ifNeededAtTime: seconds
	seconds >= sendTime ifTrue: [
		sendTime := Time localSecondClock.
		self debugLog: '*******************resendFor MQTTPacketPubRel for ' , originalPacket packetID asString.
		anMQTTClient statPerform: #resendType: with: MQTTPacketPubRel asString.
		anMQTTClient sendPacket: (MQTTPacketPubRel new packetID: originalPacket packetID)]! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPendingSubAckJob methods"!

!MQTTPendingSubAckJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
completeFor: anMQTTClient
	super completeFor: anMQTTClient.
	self debugLog: 'suback return codes need handling'! !

!MQTTPendingSubAckJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isSubAckID: msgID
	^ self packetID = msgID! !

!MQTTPendingSubAckJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
resendFor: anMQTTClient ifNeededAtTime: seconds
	anMQTTClient statPerform: #resendType: with: MQTTPacketSubAck asString! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPendingUnsubAckJob methods"!

!MQTTPendingUnsubAckJob methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isUnsubAckID: msgID
	^ self packetID = msgID! !

!MQTTPendingUnsubAckJob methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
resendFor: anMQTTClient ifNeededAtTime: seconds
	anMQTTClient statPerform: #resendType: with: MQTTPacketUnsubAck asString! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacket class methods"!

!MQTTPacket class methodsFor: 'class initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	"MQTTPacket initialize"
	PacketTypes := OrderedCollection new
		add: MQTTPacketConnect; add: MQTTPacketConnAck; add: MQTTPacketPublish;
		add: MQTTPacketPubAck; add: MQTTPacketPubRec; add: MQTTPacketPubRel;
		add: MQTTPacketPubComp; add: MQTTPacketSubscribe; add: MQTTPacketSubAck;
		add: MQTTPacketUnsubscribe; add: MQTTPacketUnsubAck; add: MQTTPacketPingReq;
		add: MQTTPacketPingResp; add: MQTTPacketDisconnect; yourself.
	MQTTProtocolLevel311 := 4! !

!MQTTPacket class methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
badPacketTypeError
	self debugLog: 'badPacketTypeError'.
	MQTTBadPacketTypeException signal! !

!MQTTPacket class methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
debugLog: aString
	MQTTClientInterface debugLog: aString tag: ' PP ' str2: ''! !

!MQTTPacket class methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
emptyStreamError
	^ MQTTEmptyStreamErrorException signal: 'MQTT no bytes available to be read'! !

!MQTTPacket class methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
readFrom: aReadStream
	| byte |
	byte := [aReadStream peek] on: ConnectionTimedOut do: [:ex | self emptyStreamError].
	byte ifNil: [^ self emptyStreamError].
	^ (PacketTypes at: byte >> 4 ifAbsent: [self badPacketTypeError]) new readFrom: aReadStream! !


"MQTTPacket instance methods"!

!MQTTPacket methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
badConnectAcknowledgeError
	self debugLog: 'badConnectAcknowledgeError'.
	MQTTConnectionException signal: 'badConnectAcknowledgeError'! !

!MQTTPacket methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
badFixedHeaderError
	self debugLog: 'badFixedHeaderError'.
	^ MQTTBadHeaderException signal! !

!MQTTPacket methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
badQosError
	self debugLog: 'badQosError'.
	MQTTBadHeaderException signal: 'badQosError'! !

!MQTTPacket methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
badTopicError
	self debugLog: 'badTopicError'.
	MQTTBadTopicException signal: 'badTopicError'! !

!MQTTPacket methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
badTopicListError
	self debugLog: 'badTopicListError'.
	MQTTBadTopicException signal: 'badTopicListError'! !

!MQTTPacket methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
connectionRefusedError: refusalCode
	self debugLog: 'connectionRefusedError:'.
	^ MQTTCONNECTException connectReturnCode: refusalCode! !

!MQTTPacket methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
debugLog: aString
	self class debugLog: aString! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decode16BitIntegerFrom: aReadStream
	^ aReadStream next << 8 + aReadStream next! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeFixedHeaderFrom: aReadStream
	| token |
	token := aReadStream next.
	token = self fixedHeader ifFalse: [self badFixedHeaderError].
	self decodeLengthFrom: aReadStream! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeFrom: aReadStream
	self decodeFixedHeaderFrom: aReadStream! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeLengthFrom: aReadStream
	| byte |
	byte := aReadStream next.
	byte > 0 ifTrue: [self encodedLengthError]! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encode16BitInteger: smallNumber on: aWriteStream
	aWriteStream
		nextPut: (smallNumber >> 8);
		nextPut: (smallNumber bitAnd: 16rFF)! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeOn: aWriteStream
	aWriteStream
		nextPut: self fixedHeader;
		nextPut: 0;
		flush.
	^ aWriteStream! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeString: aString on: aWriteStream
	| bytes |
	bytes := UTF8Encoder encode: aString asString.
	self encode16BitInteger: bytes size on: aWriteStream.
	aWriteStream nextPutAll: bytes! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/19/2026 12:00'!
decodeStringFrom: aReadStream
	"Decode a length-prefixed UTF8 string from the stream"
	| ln bytes |
	ln := self decode16BitIntegerFrom: aReadStream.
	bytes := aReadStream next: ln.
	^ UTF8Encoder decode: bytes! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeVariableLength: aLength on: aWriteStream
	"Encode a variable length integer per MQTT spec"
	| val byte |
	val := aLength.
	[byte := val \\ 128.
	val := val // 128.
	val > 0 ifTrue: [byte := byte bitOr: 128].
	aWriteStream nextPut: byte.
	val > 0] whileTrue! !

!MQTTPacket methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
encodedLengthError
	self debugLog: 'encodedLengthError'.
	MQTTBadHeaderException signal: 'encodedLengthError'! !

!MQTTPacket methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	^ self subclassResponsibility! !

!MQTTPacket methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
fixedHeader
	^ self packetType << 4 bitOr: self fixedHeaderFlags! !

!MQTTPacket methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
fixedHeaderFlags
	^ 0! !

!MQTTPacket methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ nil! !

!MQTTPacket methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
readFrom: aReadStream
	self decodeFrom: aReadStream! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketDisconnect methods"!

!MQTTPacketDisconnect methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handleDisconnectPacket: self! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 14! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketPingReq methods"!

!MQTTPacketPingReq methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTServer
	anMQTTServer handlePingReqPacket: self! !

!MQTTPacketPingReq methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 12! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketPingResp methods"!

!MQTTPacketPingResp methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handlePingRespPacket: self! !

!MQTTPacketPingResp methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 13! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketVariableHeaded methods"!

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeByteArrayFrom: aReadStream
	| ln bytes |
	ln := self decode16BitIntegerFrom: aReadStream.
	bytes := aReadStream next: ln.
	^ bytes! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeFrom: aReadStream
	super decodeFrom: aReadStream.
	self decodeVariableHeaderFrom: aReadStream! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeLengthFrom: aReadStream
	| byte shift val |
	shift := 0.
	val := 0.
	[byte := aReadStream next.
	(byte isNil or: [shift > 21]) ifTrue: [self encodedLengthError].
	val := val + ((byte bitAnd: 127) << shift).
	shift := shift + 7.
	(byte bitAnd: 128) > 0] whileTrue.
	remainingLength := val! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/19/2026 12:00'!
decodeStringFrom: aReadStream
	| ln bytes |
	ln := self decode16BitIntegerFrom: aReadStream.
	bytes := aReadStream next: ln.
	^ UTF8Encoder decode: bytes! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeVariableHeaderFrom: aReadStream
	msgID := self decode16BitIntegerFrom: aReadStream! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeFixedHeaderOn: aWriteStream
	aWriteStream nextPut: self fixedHeader.
	self encodeLengthOn: aWriteStream! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeLengthOn: aWriteStream
	| val byte |
	val := remainingLength.
	[byte := val \\ 128.
	val := val // 128.
	val > 0 ifTrue: [byte := byte bitOr: 128].
	aWriteStream nextPut: byte.
	val > 0] whileTrue! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeOn: aWriteStream
	| tempStream |
	tempStream := MQTTWriteStream on: (ByteArray new: self guessEncodedLength).
	self encodeVariableHeaderOn: tempStream.
	self encodePayloadOn: tempStream.
	remainingLength := tempStream position.
	self encodeFixedHeaderOn: aWriteStream.
	aWriteStream next: tempStream position putAll: tempStream contents startingAt: 1.
	aWriteStream flush.
	^ aWriteStream! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodePayloadOn: ignored
	"do nothing; subclasses that actually have payloads must do Their Own Thing"! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeVariableHeaderOn: aWriteStream
	self encode16BitInteger: msgID on: aWriteStream! !

!MQTTPacketVariableHeaded methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
guessEncodedLength
	^ 100! !

!MQTTPacketVariableHeaded methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetID
	^ msgID! !

!MQTTPacketVariableHeaded methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetID: a16BitNumber
	msgID := a16BitNumber! !

!MQTTPacketVariableHeaded methodsFor: 'printing' stamp: 'JMM 1/18/2026 12:00'!
printOn: aStream
	super printOn: aStream.
	msgID ifNotNil: [
		aStream nextPutAll: ' msgID: '.
		msgID printOn: aStream]! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketVariableHeadedWithPayload methods"!

!MQTTPacketVariableHeadedWithPayload methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeFrom: aReadStream
	super decodeFrom: aReadStream.
	self decodePayloadFrom: aReadStream! !

!MQTTPacketVariableHeadedWithPayload methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodePayloadFrom: aReadStream
	"default version does nothing yet"! !

!MQTTPacketVariableHeadedWithPayload methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodePayloadOn: aWriteStream
	"default version does nothing yet"! !

!MQTTPacketVariableHeadedWithPayload methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
messageID
	^ msgID! !

!MQTTPacketVariableHeadedWithPayload methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
messageID: aNumber
	msgID := aNumber! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketConnAck methods"!

!MQTTPacketConnAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
byte1
	^ byte1! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
byte2
	^ byte2! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
byte2: aValue
	byte2 := aValue! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
returnCode
	"Alias for byte2 - the MQTT connect return code"
	^ byte2! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
returnCode: aValue
	"Alias for byte2: - set the MQTT connect return code"
	byte2 := aValue! !

!MQTTPacketConnAck methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeVariableHeaderFrom: aReadStream
	byte1 := aReadStream next.
	(byte1 anyMask: 16rFE) ifTrue: [^ self badConnectAcknowledgeError].
	sessionPresent := byte1 allMask: 1.
	byte2 := aReadStream next! !

!MQTTPacketConnAck methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeVariableHeaderOn: aWriteStream
	byte1 := sessionPresent asBit.
	aWriteStream
		nextPut: sessionPresent asBit;
		nextPut: byte2! !

!MQTTPacketConnAck methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handleConnAckPacket: self! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 2! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
sessionPresent
	^ sessionPresent! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
sessionPresent: aValue
	sessionPresent := aValue! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketPubAck methods"!

!MQTTPacketPubAck methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handlePubAckPacket: self! !

!MQTTPacketPubAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 4! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketPubRec methods"!

!MQTTPacketPubRec methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
acknowledgement
	^ MQTTPacketPubRel new packetID: msgID! !

!MQTTPacketPubRec methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handlePubRecPacket: self! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
originalPacket
	^ originalPacket! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
originalPacket: aValue
	originalPacket := aValue! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 5! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketPubRel methods"!

!MQTTPacketPubRel methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
acknowledgement
	^ MQTTPacketPubComp new packetID: msgID! !

!MQTTPacketPubRel methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handlePubRelPacket: self! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
fixedHeaderFlags
	^ 2! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 6! !

!MQTTPacketPubRel methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
pendingJob
	^ MQTTPendingPubCompJob for: self! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketPubComp methods"!

!MQTTPacketPubComp methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handlePubCompPacket: self! !

!MQTTPacketPubComp methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 7! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketUnsubAck methods"!

!MQTTPacketUnsubAck methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handleUnsubAckPacket: self! !

!MQTTPacketUnsubAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 11! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketConnect methods"!

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
cleanSessionFlag: sessionBoolean
	cleanSessionFlag := sessionBoolean! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
clientID: clientIDString
	clientID := clientIDString! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
clientIDString
	^ clientID ifNil: ['CuisMQTT']! !

!MQTTPacketConnect methodsFor: 'private' stamp: 'JMM 1/18/2026 12:00'!
connectFlags
	| byte |
	byte := willQos << 3.
	byte := byte bitAt: 2 put: cleanSessionFlag asBit.
	byte := byte bitAt: 3 put: willTopic notNil asBit.
	byte := byte bitAt: 6 put: willRetainFlag asBit.
	byte := byte bitAt: 7 put: password notNil asBit.
	byte := byte bitAt: 8 put: user notNil asBit.
	^ byte! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodePayloadFrom: aReadStream
	| cFlags passwordFlag userFlag protocol version |
	protocol := self decodeStringFrom: aReadStream.
	protocol = 'MQTT' ifFalse: [
		self debugLog: 'Protocol found: ' , protocol.
		(self connectionRefusedError: 16r01) signal].
	version := aReadStream next.
	(version = 3 or: [version = 4]) ifFalse: [self debugLog: 'version found: ' , version].
	cFlags := aReadStream next.
	(cFlags allMask: 1) ifTrue: [(self connectionRefusedError: 16r01) signal].
	cleanSessionFlag := cFlags allMask: 2.
	willFlag := cFlags allMask: 4.
	willRetainFlag := cFlags allMask: 1 << 5.
	willQos := cFlags >> 3 bitAnd: 3.
	passwordFlag := cFlags allMask: 1 << 6.
	userFlag := cFlags allMask: 1 << 7.
	keepAliveTime := self decode16BitIntegerFrom: aReadStream.
	clientID := self decodeStringFrom: aReadStream.
	clientID size = 0 ifTrue: [(self connectionRefusedError: 16r02) signal].
	willFlag ifTrue: [
		willTopic := self decodeStringFrom: aReadStream.
		willMessage := self decodeByteArrayFrom: aReadStream].
	userFlag ifTrue: [user := self decodeStringFrom: aReadStream].
	passwordFlag ifTrue: [password := self decodeByteArrayFrom: aReadStream].
	returnCode := 16r00! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeVariableHeaderFrom: aReadStream
	"do nothing because the actual work is deferred to the decodePayloadFrom: method"! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodePayloadOn: aWriteStream
	self encodeString: self clientIDString on: aWriteStream.
	self encodeWillDataOn: aWriteStream.
	self encodeUsernameOn: aWriteStream.
	self encodePasswordOn: aWriteStream! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodePasswordOn: aWriteStream
	password ifNotNil: [self encodeString: password on: aWriteStream]! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeUsernameOn: aWriteStream
	user ifNotNil: [self encodeString: user on: aWriteStream]! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeVariableHeaderOn: aWriteStream
	self encodeString: 'MQTT' on: aWriteStream.
	aWriteStream
		nextPut: MQTTProtocolLevel311;
		nextPut: self connectFlags.
	self encode16BitInteger: keepAliveTime on: aWriteStream! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeWillDataOn: aWriteStream
	willTopic ifNotNil: [
		self encodeString: willTopic on: aWriteStream.
		willMessage ifNil: [self encodeString: '' on: aWriteStream]
			ifNotNil: [self encodeString: willMessage on: aWriteStream]]! !

!MQTTPacketConnect methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTServer
	^ anMQTTServer handleConnectPacket: self! !

!MQTTPacketConnect methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	willRetainFlag := false.
	cleanSessionFlag := true.
	willQos := 0.
	keepAliveTime := 60! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
keepAliveTime
	^ keepAliveTime! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
keepAliveTime: timeInSecs
	keepAliveTime := timeInSecs! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 1! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
user
	^ user! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
user: userName
	user := userName! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
password
	^ password! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
password: passwd
	password := passwd! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
user: userName password: passwd
	user := userName.
	password := passwd! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
willTopic: topicString message: messageString retain: retainBoolean qos: qosValue
	willTopic := topicString.
	willMessage := messageString.
	willRetainFlag := retainBoolean.
	willQos := qosValue min: 2 max: 0! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketPublish methods"!

!MQTTPacketPublish methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
acknowledgement
	| pubRec |
	qos = 0 ifTrue: [^ self badQosError].
	qos = 1 ifTrue: [^ MQTTPacketPubAck new packetID: msgID].
	pubRec := MQTTPacketPubRec new.
	pubRec packetID: msgID.
	pubRec originalPacket: self.
	^ pubRec! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeFixedHeaderFrom: aReadStream
	| hdrByte |
	(hdrByte := aReadStream next) >> 4 = self packetType ifFalse: [self badFixedHeaderError].
	duplicate := hdrByte allMask: 8.
	retain := hdrByte allMask: 1.
	qos := (hdrByte >> 1) bitAnd: 3.
	self decodeLengthFrom: aReadStream! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeFrom: aReadStream
	| ln |
	self decodeFixedHeaderFrom: aReadStream.
	ln := self decodeVariableHeaderFrom: aReadStream.
	message := aReadStream next: (remainingLength - ln)! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: 'JMM 1/19/2026 12:00'!
decodeTopicStringFrom: aReadStream
	| ln bytes |
	ln := self decode16BitIntegerFrom: aReadStream.
	bytes := aReadStream next: ln.
	topic := [UTF8Encoder decode: bytes] on: Error do: [:ex | 'Failed Topic Decode'].
	^ ln + 2! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodeVariableHeaderFrom: aReadStream
	| ln |
	ln := self decodeTopicStringFrom: aReadStream.
	qos > 0 ifTrue: [
		super decodeVariableHeaderFrom: aReadStream.
		ln := ln + 2].
	^ ln! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
duplicateFlag: aBoolean
	duplicate := aBoolean! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodePayloadOn: aWriteStream
	aWriteStream nextPutAll: message! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeVariableHeaderOn: aWriteStream
	topic ifNil: [^ self badTopicError].
	self encodeString: topic on: aWriteStream.
	qos > 0 ifTrue: [super encodeVariableHeaderOn: aWriteStream]! !

!MQTTPacketPublish methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handlePublishPacket: self qos: qos! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
fixedHeaderFlags
	^ ((duplicate ifTrue: [8] ifFalse: [0]) bitOr: (qos << 1)) bitOr: (retain ifTrue: [1] ifFalse: [0])! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
guessEncodedLength
	^ self message size + 100! !

!MQTTPacketPublish methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	qos := 0.
	duplicate := retain := false! !

!MQTTPacketPublish methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
matchesSubscription: subscription ifTrue: aBlock
	(subscription topicMatches: topic) ifTrue: [
		aBlock value: topic value: message value: qos]! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
message
	^ message! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
message: aByteArray
	message := aByteArray! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/19/2026 12:00'!
messageString
	^ UTF8Encoder decode: message! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 3! !

!MQTTPacketPublish methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
pendingJob
	qos = 0 ifTrue: [^ self badQosError].
	qos = 1 ifTrue: [^ MQTTPendingPubAckJob for: self].
	^ MQTTPendingPubRecJob for: self! !

!MQTTPacketPublish methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
prepareForResend
	duplicate := true! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
qos
	^ qos! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
qos: aValue
	qos := aValue! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
retain: aBoolean
	"Alias for retainFlag: for convenience"
	retain := aBoolean! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
retainFlag: aBoolean
	retain := aBoolean! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
topic
	^ topic! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
topic: topicString
	"Set just the topic - use with message: for separate setting"
	((UTF8Encoder encode: topicString asString) size > 65535 or: [topicString includesAnyOf: #($# $+)])
		ifTrue: [^ self badTopicError].
	topic := topicString! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
topic: topicString message: messageBytes
	((UTF8Encoder encode: topicString asString) size > 65535 or: [topicString includesAnyOf: #($# $+)])
		ifTrue: [^ self badTopicError].
	topic := topicString.
	message := messageBytes! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketSubscribe methods"!

!MQTTPacketSubscribe methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
addTopic: aString qos: aSmallNumber
	payloadDict at: aString put: (aSmallNumber min: 2 max: 0)! !

!MQTTPacketSubscribe methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodePayloadFrom: aReadStream
	| topicStr qosVal estimatedEnd |
	estimatedEnd := remainingLength - 2.
	[topicStr := self decodeStringFrom: aReadStream.
	(topicStr isNil or: [topicStr size = 0]) ifTrue: [self badTopicError].
	qosVal := aReadStream next asInteger.
	payloadDict at: topicStr put: qosVal.
	estimatedEnd := estimatedEnd - (topicStr size) - 2 - 1.
	estimatedEnd = 0] whileFalse! !

!MQTTPacketSubscribe methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeOn: aWriteStream
	payloadDict ifEmpty: [^ self badTopicListError].
	^ super encodeOn: aWriteStream! !

!MQTTPacketSubscribe methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodePayloadOn: aWriteStream
	payloadDict keysAndValuesDo: [:key :val |
		self encodeString: key on: aWriteStream.
		aWriteStream nextPut: (val bitAnd: 3)]! !

!MQTTPacketSubscribe methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTServer
	^ anMQTTServer handleSubscribePacket: self! !

!MQTTPacketSubscribe methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
fixedHeaderFlags
	^ 2! !

!MQTTPacketSubscribe methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	payloadDict := Dictionary new! !

!MQTTPacketSubscribe methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 8! !

!MQTTPacketSubscribe methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
payloadDict
	^ payloadDict! !

!MQTTPacketSubscribe methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
topics
	"Return the topics as a collection (keys of payloadDict)"
	^ payloadDict keys asOrderedCollection! !

!MQTTPacketSubscribe methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
pendingJob
	^ MQTTPendingSubAckJob new originalPacket: self! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketSubAck methods"!

!MQTTPacketSubAck methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodePayloadFrom: aReadStream
	returnCodes := aReadStream next: (remainingLength - 2)! !

!MQTTPacketSubAck methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodePayloadOn: aWriteStream
	aWriteStream nextPutAll: self returnCodes! !

!MQTTPacketSubAck methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handleSubAckPacket: self! !

!MQTTPacketSubAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 9! !

!MQTTPacketSubAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
returnCodes
	^ returnCodes! !

!MQTTPacketSubAck methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
returnCodes: aValue
	returnCodes := aValue! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTPacketUnsubscribe methods"!

!MQTTPacketUnsubscribe methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
addTopic: aTopic
	topics add: aTopic! !

!MQTTPacketUnsubscribe methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
decodePayloadFrom: aReadStream
	| topicStr estimatedEnd |
	estimatedEnd := remainingLength - 2.
	[topicStr := self decodeStringFrom: aReadStream.
	(topicStr isNil or: [topicStr size = 0]) ifTrue: [self badTopicError].
	topics add: topicStr.
	estimatedEnd := estimatedEnd - (topicStr size) - 2.
	estimatedEnd = 0] whileFalse! !

!MQTTPacketUnsubscribe methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodeOn: aWriteStream
	topics ifEmpty: [^ self badTopicListError].
	^ super encodeOn: aWriteStream! !

!MQTTPacketUnsubscribe methodsFor: 'encoding' stamp: 'JMM 1/18/2026 12:00'!
encodePayloadOn: aWriteStream
	topics do: [:val | self encodeString: val on: aWriteStream]! !

!MQTTPacketUnsubscribe methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
evaluateFor: anMQTTClient
	anMQTTClient handleUnsubscribePacket: self! !

!MQTTPacketUnsubscribe methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
fixedHeaderFlags
	^ 2! !

!MQTTPacketUnsubscribe methodsFor: 'initialization' stamp: 'JMM 1/18/2026 12:00'!
initialize
	topics := OrderedCollection new! !

!MQTTPacketUnsubscribe methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetType
	^ 10! !

!MQTTPacketUnsubscribe methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
pendingJob
	^ MQTTPendingUnsubAckJob for: self! !

!MQTTPacketUnsubscribe methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
topics
	^ topics! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTTransportLayer methods"!

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
abort
	^ abort! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
addPending: mqttJobToComplete
	pendingJobsMutex critical: [
		pendingJobs ifNil: [pendingJobs := OrderedCollection new].
		pendingJobs add: mqttJobToComplete]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
badPacketID
	self debugLog: 'badPacketID'.
	self disconnect: false! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
badTopicError
	self debugLog: 'badTopicError'.
	MQTTBadTopicException signal: 'badTopicError'! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
currentSubscriptions
	^ currentSubscriptions! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
currentSubscriptions: aValue
	currentSubscriptions := aValue! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
debugLog: aString
	| datum |
	datum := sockStrm ifNotNil: [sockStrm socket printString] ifNil: ['???'].
	MQTTClientInterface debugLog: datum tag: self transportType str2: aString! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
disconnect: restart
	[(Delay forSeconds: 2) wait.
	outgoingPacketProcess notNil ifTrue: [outgoingPacketProcess terminate].
	self debugLog: 'kill outgoingPacketProcess'] forkAt: Processor activePriority - 1.
	outgoingPacketProcess := nil.
	[(Delay forSeconds: 2) wait.
	incomingPacketProcess notNil ifTrue: [incomingPacketProcess terminate].
	self debugLog: 'kill incomingPacketProcess'] forkAt: Processor activePriority - 1.
	incomingPacketProcess := nil.
	retryProcess ifNotNil: [
		self debugLog: 'kill retryProcess'.
		retryProcess terminate.
		retryProcess := nil]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
disconnectPostCleanup
	| s |
	s := sockStrm socket.
	[s ifNotNil: [[s close] on: Error do: [:ex | self debugLog: ex description]]] fork! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/19/2026 12:00'!
exceptionHandler: e
	| connectAckPacket et sock |
	e class = MQTTEmptyStreamErrorException ifTrue: [
		"Check if socket is still usable before retrying"
		sock := sockStrm ifNotNil: [sockStrm socket].
		(sock notNil and: [sock isConnected])
			ifTrue: [
				self debugLog: 'MQTTEmptyStreamErrorException'.
				^ e retry]
			ifFalse: [
				self debugLog: 'MQTTEmptyStreamErrorException - socket closed'.
				self disconnect: false.
				^ nil]].
	e class = ConnectionTimedOut ifTrue: [self debugLog: 'ConnectionTimedOut'].
	et := e printString.
	et ifNotNil: [
		self debugLog: et.
		self statPerform: #errorTypes: with: et].
	e class = MQTTCONNECTException ifTrue: [
		self statPerform: #badConnections.
		connectAckPacket := MQTTPacketConnAck new.
		connectAckPacket sessionPresent: false.
		connectAckPacket byte2: e connectReturnCode.
		self sendPacket: connectAckPacket.
		(Delay forSeconds: 5) wait].
	self disconnect: self restartTheSocket.
	^ nil! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
findAndRunJobsOverdueForAcknowledgement
	| tick |
	pendingJobs ifNil: [^ nil].
	tick := Time localSecondClock - retryTime.
	pendingJobsMutex critical: [pendingJobs do: [:j | j resendFor: self ifNeededAtTime: tick]]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
findPendingJob: testBlock
	pendingJobs ifNil: [^ nil].
	pendingJobsMutex critical: [^ pendingJobs detect: [:e | testBlock value: e] ifNone: [nil]]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handleIncomingPacket: anMQTTPacket
	anMQTTPacket evaluateFor: self! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handlePubAckPacket: aPubAckPacket
	| job |
	job := self findPendingJob: [:j | j isPubAckID: aPubAckPacket packetID].
	job ifNil: [^ self missingPendingJob: #pubAck].
	job completeFor: self! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handlePubCompPacket: aPubCompPacket
	| job |
	job := self findPendingJob: [:j | j isPubCompID: aPubCompPacket packetID].
	job ifNil: [^ self missingPendingJob: #pubComp].
	job completeFor: self! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handlePubRecPacket: aPubRecPacket
	| job pubRelPacket pj |
	job := self findPendingJob: [:j | j isPubRecID: aPubRecPacket packetID].
	job ifNil: [self missingPendingJob: #pubRec].
	pubRelPacket := aPubRecPacket acknowledgement.
	self addPending: (pj := pubRelPacket pendingJob).
	self sendPacket: (MQTTPacketAndPendingJobPair packet: pubRelPacket pendingJob: pj).
	job ifNotNil: [job completeFor: self]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handlePubRelPacket: aPubRelPacket
	| job |
	job := self findPendingJob: [:j | j isPubRelID: aPubRelPacket packetID].
	job ifNil: [self missingPendingJob: #pubRel].
	self sendPacket: aPubRelPacket acknowledgement.
	job ifNotNil: [
		job completeFor: self.
		self doSubscriptionActionsFor: job originalPacket originalPacket]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handlePublishPacket: aPublishPacket qos: packetQos
	| pubRecOrAckPacket pj |
	packetQos >= 1 ifTrue: [
		pubRecOrAckPacket := aPublishPacket acknowledgement.
		packetQos = 1 ifTrue: [self sendPacket: pubRecOrAckPacket]].
	packetQos = 2 ifTrue: [
		(self findPendingJob: [:j | j isPubRelID: aPublishPacket packetID]) ifNotNil: [^ self].
		self addPending: (pj := MQTTPendingPubRelJob for: pubRecOrAckPacket).
		self sendPacket: (MQTTPacketAndPendingJobPair packet: pubRecOrAckPacket pendingJob: pj).
		^ self].
	self doSubscriptionActionsFor: aPublishPacket! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handleSubAckPacket: aSubAckPacket
	| job |
	job := self findPendingJob: [:j | j isSubAckID: aSubAckPacket packetID].
	job ifNil: [^ self missingPendingJob: #subAck].
	job completeFor: self! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handleUnsubAckPacket: anUnsubAckPacket
	| job |
	job := self findPendingJob: [:j | j isUnsubAckID: anUnsubAckPacket packetID].
	job ifNil: [^ self missingPendingJob: #unSubAck].
	job completeFor: self! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
initializeKeepAlive
	"keep alive action - subclass responsibility"! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
initializeMQTTConnection: anObject
	"assemble a CONNECT packet and send it - subclass responsibility"! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
initializePacketReading
	| innerBlock |
	innerBlock := self packetReader.
	incomingPacketProcess := [
		[| packet |
		packet := [innerBlock value] on: Error do: [:e | self exceptionHandler: e].
		packet ifNotNil: [
			self statPerform: #packetIn: with: packet.
			(packet packetType = 3 and: [packet qos > 0]) ifTrue: [self debugLog: 'Rx ' , packet asString].
			[self handleIncomingPacket: packet] on: Error do: [:e | self exceptionHandler: e]].
		Processor yield.
		self abort] whileFalse] fork! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
initializePacketWriting
	| innerBlock |
	innerBlock := self packetWriter.
	outgoingPacketProcess := [
		[| packet |
		packet := self packetInFlightQueue nextOrNil.
		packet ifNil: [packet := self outgoingPacketQueue next].
		self packetInFlightQueue nextPut: packet.
		self outgoingPacketQueue size > 10 ifTrue: [
			self debugLog: 'write packet backlog at: ' , self outgoingPacketQueue size printString].
		[innerBlock value: packet] on: Error do: [:e | self exceptionHandler: e].
		self abort ifFalse: [self packetInFlightQueue nextOrNil].
		Processor yield.
		self abort] whileFalse] fork! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
initializeRetryProcess
	retryProcess := [
		| loopDelay |
		loopDelay := Delay forSeconds: 2.
		[self abort ifFalse: [self findAndRunJobsOverdueForAcknowledgement].
		loopDelay wait.
		self abort] whileFalse] fork! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
initializeSocketStream: socket
	"Adapted for Cuis - use SocketStream"
	sockStrm := SocketStream on: socket.
	sockStrm binary.
	sockStrm timeout: self readWaitTime! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
lastMID
	^ lastMID! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
lastMID: aValue
	lastMID := aValue! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
missingPendingJob: type
	self debugLog: '************ No matching pending job for type: ' , type asString.
	self statPerform: #missingType: with: type! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
newPacketID
	| prevMID pj |
	lastMID ifNil: [lastMID := 1].
	prevMID := lastMID.
	[lastMID := lastMID + 1 \\ 16rFFFF.
	lastMID = 0 ifTrue: [lastMID := 1].
	pendingJobsMutex critical: [
		pj := pendingJobs.
		pj ifNil: [^ lastMID] ifNotNil: [pj detect: [:jb | jb packetID = lastMID] ifNone: [^ lastMID]]].
	prevMID = lastMID] whileFalse.
	^ self badPacketID! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
onTopic: topicString qos: qos do: aBlock
	| subscription |
	subscription := MQTTSubscription for: topicString qos: qos do: aBlock.
	subscription ifNil: [^ self badTopicError].
	currentSubscriptions add: subscription.
	self subscribe: topicString qos: qos! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
outgoingPacketQueue
	^ outgoingPacketQueue! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
outgoingPacketQueue: aQueue
	outgoingPacketQueue := aQueue! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetInFlightQueue
	^ packetInFlightQueue! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
packetInFlightQueue: aValue
	packetInFlightQueue := aValue! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
packetReader
	^ [MQTTPacket readFrom: sockStrm]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
packetWriter
	^ [:packet |
		self debugLog: 'Wx ' , packet asString.
		packet encodeOn: sockStrm.
		self statPerform: #packetOut: with: packet]! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
pendingJobs
	^ pendingJobs! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
pendingJobs: aValue
	pendingJobs := aValue! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
preambleWorkAbortEarlyIfTrue: restart
	abortMutex critical: [
		abort ifTrue: [^ true].
		self debugLog: 'disconnecting with restart: ' , restart printString.
		self statPerform: #disconnect.
		self sendPossibleDisconnectPackage.
		abort := true.
		self disconnectPostCleanup].
	^ false! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
publishTopic: aTopic message: msgString qos: qos retain: retainFlag
	| pubPacket pj |
	pubPacket := MQTTPacketPublish new
		topic: aTopic message: msgString asByteArray;
		retainFlag: retainFlag;
		qos: qos.
	qos > 0 ifTrue: [
		pubPacket messageID: self newPacketID.
		self addPending: (pj := pubPacket pendingJob)].
	self sendPacket: (MQTTPacketAndPendingJobPair packet: pubPacket pendingJob: pj)! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
readWaitTime
	^ readWaitTime! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
readWaitTime: aValue
	readWaitTime := aValue! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
releasePendingJob: pendingJob
	pendingJobs ifNil: [^ nil].
	pendingJobsMutex critical: [^ pendingJobs remove: pendingJob ifAbsent: [nil]]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
restart: aSocket
	(Delay forSeconds: 3) wait.
	self debugLog: 'reStart Socket'.
	abortMutex critical: [abort := false].
	self initializeSocketStream: aSocket.
	self
		initializeMQTTConnection: false;
		initializePacketReading;
		initializePacketWriting;
		initializeRetryProcess! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
restartTheSocket
	^ true! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
retryTime
	^ retryTime! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
sendPacket: anMQTTPacket
	outgoingPacketQueue nextPut: anMQTTPacket! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
sendPossibleDisconnectPackage
	"do nothing"! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
sockStrm
	^ sockStrm! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
sockStrm: anObject
	sockStrm := anObject! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
start: aSocket
	self debugLog: 'Start Socket'.
	abort := false.
	retryTime := 20.
	readWaitTime := 30.
	pendingJobsMutex := Semaphore forMutualExclusion.
	currentSubscriptions := OrderedCollection new.
	outgoingPacketQueue := SharedQueue new.
	packetInFlightQueue := SharedQueue new.
	statistics := MQTTStatistics new.
	statisticsMutex := Semaphore forMutualExclusion.
	abortMutex := Semaphore forMutualExclusion.
	self initializeSocketStream: aSocket.
	self
		initializeMQTTConnection: true;
		initializePacketReading;
		initializePacketWriting;
		initializeRetryProcess.
	self debugLog: 'Started Socket'! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
statPerform: selector
	statisticsMutex critical: [[statistics perform: selector] on: Error do: [:ex | ]]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
statPerform: selector with: arg
	statisticsMutex critical: [[statistics perform: selector with: arg] on: Error do: [:ex | ]]! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
subscribe: aTopic qos: qos
	| subPacket pj |
	subPacket := MQTTPacketSubscribe new
		addTopic: aTopic qos: qos;
		packetID: self newPacketID.
	pj := subPacket pendingJob.
	self addPending: pj.
	self sendPacket: (MQTTPacketAndPendingJobPair packet: subPacket pendingJob: pj)! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
transportType
	^ ' TL '! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTTransportLayerClient class methods"!

!MQTTTransportLayerClient class methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
clientID
	"Generate a unique client ID using timestamp and random number"
	ClientID ifNil: [
		ClientID := 'Cuis-', Time localSecondClock printString, '-', (1000000 atRandom) printString].
	^ ClientID! !


"MQTTTransportLayerClient instance methods"!

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
clientID: clientIDString
	clientID := clientIDString! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
clientIDString
	^ clientID ifNil: [self class clientID]! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
disconnect: restart
	(self preambleWorkAbortEarlyIfTrue: restart) ifTrue: [^ self].
	super disconnect: restart.
	restart ifTrue: [
		[| itf |
		itf := self socketClient interface.
		itf ifNotNil: [itf restart]] fork]! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
disconnectPostCleanup
	super disconnectPostCleanup! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
doSubscriptionActionsFor: aPublishPacket
	currentSubscriptions copy do: [:s | s handlePacket: aPublishPacket]! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handleConnAckPacket: aConnAckPacket
	aConnAckPacket byte2 > 0 ifTrue: [MQTTConnectionException signal]! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
handlePingRespPacket: aPingRespPacket
	pingOK := true! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
initializeKeepAlive
	keepAliveTime = 0 ifFalse: [self addPending: MQTTPendingPingJob new]! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
keepAliveTime: timeInSecs
	keepAliveTime := timeInSecs! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
ping
	pingOK := false.
	self sendPacket: MQTTPacketPingReq new.
	^ Time localSecondClock + keepAliveTime! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
restart: aSocket
	outgoingPacketQueue := outgoingPacketQueue copy flushAllSuchThat: [:p |
		p class = MQTTPacketDisconnect or: [p class = MQTTPacketConnect]].
	packetInFlightQueue := packetInFlightQueue copy flushAllSuchThat: [:p |
		p class = MQTTPacketDisconnect or: [p class = MQTTPacketConnect]].
	super restart: aSocket.
	self initializeKeepAlive! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
sendPossibleDisconnectPackage
	self sendPacket: MQTTPacketDisconnect new.
	Processor yield.
	(Delay forSeconds: 2) wait! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
socketClient
	^ socketClient! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
socketClient: aValue
	socketClient := aValue! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
start: aSocket
	super start: aSocket.
	self initializeKeepAlive! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
subscribe: aTopic qos: qos
	| subPacket pj |
	subPacket := MQTTPacketSubscribe new
		addTopic: aTopic qos: qos;
		packetID: self newPacketID.
	self addPending: (pj := subPacket pendingJob).
	self sendPacket: (MQTTPacketAndPendingJobPair packet: subPacket pendingJob: pj)! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
transportType
	^ ' CC '! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
unsubscribe: aTopic
	| unsubPacket pj |
	unsubPacket := MQTTPacketUnsubscribe new
		addTopic: aTopic;
		packetID: self newPacketID.
	self addPending: (pj := unsubPacket pendingJob).
	self sendPacket: (MQTTPacketAndPendingJobPair packet: unsubPacket pendingJob: pj)! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
unsubscribeFrom: aTopic
	currentSubscriptions removeAllSuchThat: [:s | s topicMatches: aTopic].
	self unsubscribe: aTopic! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTSocketClient class methods"!

!MQTTSocketClient class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
openOnHostName: hostname keepAlive: aSeconds interface: anInterface
	^ self openOnHostName: hostname port: 1883 keepAlive: aSeconds interface: anInterface! !

!MQTTSocketClient class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
openOnHostName: hostname port: portNumber keepAlive: aSeconds interface: anInterface
	"Create an instance on the provided host and port - Adapted for Cuis.
	Does NOT start MQTT handshake - call connect after setting credentials."
	| instance sock addr |
	addr := NetNameResolver addressForName: hostname timeout: aSeconds.
	instance := self new.
	sock := Socket newTCP.
	sock connectTo: addr port: portNumber.
	sock waitForConnectionFor: aSeconds.
	instance keepAliveTime: aSeconds.
	instance interface: anInterface.
	instance pendingSocket: sock.
	^ instance! !


"MQTTSocketClient instance methods"!

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
acceptFrom: aSocket
	self debugLog: 'MQTTSocketClient saw connectedOn: on socket'.
	transport isNil ifTrue: [
		transport := MQTTTransportLayerClient new.
		transport keepAliveTime: self keepAliveTime.
		transport socketClient: self.
		pendingClientID ifNotNil: [transport clientID: pendingClientID].
		transport start: aSocket.
		self initializeMQTTConnection: true]
	ifFalse: [
		transport restart: aSocket.
		self initializeMQTTConnection: false]! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
close
	self debugLog: 'MQTTSocketClient saw close on socket'.
	self transport ifNotNil: [self transport disconnect: true].
	self release! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
debugLog: aString
	MQTTClientInterface debugLog: self printString tag: ' TC ' str2: aString! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
disconnect
	self transport ifNotNil: [self transport disconnect: false]! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
initializeMQTTConnection: cleanSessionTrue
	| connectPacket |
	connectPacket := MQTTPacketConnect new keepAliveTime: self keepAliveTime.
	(willTopic isString & willMessage isString & (willRetainFlag isKindOf: Boolean) & willQos isInteger)
		ifTrue: [connectPacket willTopic: willTopic message: willMessage retain: willRetainFlag qos: willQos].
	cleanSessionFlag := cleanSessionTrue.
	connectPacket cleanSessionFlag: cleanSessionFlag.
	connectPacket user: userNameString password: passwordString.
	connectPacket clientID: self transport clientIDString.
	cleanSessionTrue ifTrue: [
		self transport outgoingPacketQueue: SharedQueue new.
		self transport packetInFlightQueue: SharedQueue new.
		self transport sendPacket: connectPacket]
	ifFalse: [
		| pifq |
		pifq := SharedQueue new.
		pifq nextPut: connectPacket.
		pifq nextPutAll: self transport packetInFlightQueue contents.
		self transport packetInFlightQueue: pifq]! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
interface
	^ interface! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
interface: aValue
	interface := aValue.
	transport := interface socketClient ifNotNil: [transport := interface socketClient transport]! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
pendingSocket
	^ pendingSocket! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
pendingSocket: aSocket
	pendingSocket := aSocket! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
clientID: aString
	"Set client ID to use when transport is created"
	pendingClientID := aString! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
connect
	"Start MQTT handshake on the pending socket. Call after setting credentials."
	pendingSocket ifNil: [self error: 'No pending socket - was openOnHostName called?'].
	self acceptFrom: pendingSocket.
	pendingSocket := nil! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
keepAliveTime
	^ keepAliveTime! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
keepAliveTime: aValue
	keepAliveTime := aValue! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
onTopic: topicString qos: qos do: aBlock
	self transport onTopic: topicString qos: qos do: aBlock! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
publishTopic: aTopic message: msgString qos: qos retain: retainFlag
	self transport publishTopic: aTopic message: msgString qos: qos retain: retainFlag! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
readWaitTime: aSeconds
	self transport readWaitTime: aSeconds! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
release
	interface := nil.
	transport := nil! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
transport
	^ transport! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
transport: aValue
	transport := aValue! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
unsubscribeFrom: aTopic
	self transport unsubscribeFrom: aTopic! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
username: uName password: pwd
	uName ifNotNil: [userNameString := UTF8Encoder encode: uName asString].
	pwd ifNotNil: [passwordString := UTF8Encoder encode: pwd asString]! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
willTopic: topicString message: messageString retain: retainBoolean qos: qosValue
	((topicString isString & messageString isString & (retainBoolean = true | retainBoolean = false)) & (qosValue isInteger & (qosValue between: 0 and: 2)))
		ifFalse: [^ self error: 'Bad Will'].
	willTopic := topicString.
	willMessage := messageString.
	willRetainFlag := retainBoolean.
	willQos := qosValue! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"MQTTClientInterface class methods"!

!MQTTClientInterface class methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
debugLog: aString
	self debugLog: aString tag: ' CI ' str2: ''! !

!MQTTClientInterface class methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
debugLog: aString tag: aTag str2: str2
	| stream |
	(aTag = ' CI ' or: [true]) ifFalse: [^ self].
	stream := WriteStream on: (String new: 128).
	Time now print24: true showSeconds: true on: stream.
	stream
		nextPutAll: aTag;
		nextPutAll: aString;
		space;
		nextPutAll: str2.
	Transcript show: stream contents; cr! !

!MQTTClientInterface class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
openOnHostName: hostname
	^ self openOnHostName: hostname keepAlive: 0! !

!MQTTClientInterface class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
openOnHostName: hostname keepAlive: aSeconds
	| instance minSeconds |
	instance := self new.
	minSeconds := aSeconds > 0 ifTrue: [aSeconds max: 60] ifFalse: [0].
	^ instance openOnHostName: hostname keepAlive: minSeconds! !

!MQTTClientInterface class methodsFor: 'instance creation' stamp: 'JMM 1/18/2026 12:00'!
openOnHostName: hostname port: portNumber keepAlive: aSeconds
	| instance minSeconds |
	instance := self new.
	minSeconds := aSeconds > 0 ifTrue: [aSeconds max: 60] ifFalse: [0].
	^ instance openOnHostName: hostname port: portNumber keepAlive: minSeconds! !


"MQTTClientInterface instance methods"!

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
debugLog: aString
	self class debugLog: aString! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
connect
	"Start MQTT handshake. Call after setting username/password."
	self socketClient connect! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
disconnect
	self socketClient ifNotNil: [:s | s disconnect]! !

!MQTTClientInterface methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
hostName
	^ hostName! !

!MQTTClientInterface methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
hostName: aValue
	hostName := aValue! !

!MQTTClientInterface methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isConnected
	"Return true if connected to broker"
	^ self isValid! !

!MQTTClientInterface methodsFor: 'testing' stamp: 'JMM 1/18/2026 12:00'!
isValid
	^ (socketClient notNil and: [socketClient transport notNil]) and: [socketClient transport abort not]! !

!MQTTClientInterface methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
keepAliveTime
	^ keepAliveTime ifNil: [keepAliveTime := 0]! !

!MQTTClientInterface methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
keepAliveTime: aValue
	keepAliveTime := aValue! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
openOnHostName: aHostName
	self hostName: aHostName.
	self socketClient: (self socketClientClass openOnHostName: aHostName keepAlive: keepAliveTime interface: self).
	^ self! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
openOnHostName: aHostName keepAlive: aSeconds
	self hostName: aHostName.
	self keepAliveTime: aSeconds.
	self socketClient: (self socketClientClass openOnHostName: aHostName keepAlive: aSeconds interface: self).
	^ self! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
openOnHostName: aHostName port: portNumber keepAlive: aSeconds
	self hostName: aHostName.
	self keepAliveTime: aSeconds.
	self socketClient: (self socketClientClass openOnHostName: aHostName port: portNumber keepAlive: aSeconds interface: self).
	^ self! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
publish: aByteArray onTopic: topicString qos: qos
	self publish: aByteArray onTopic: topicString qos: qos retain: false! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
publish: aByteArray onTopic: topicString qos: qos retain: retainFlag
	self socketClient publishTopic: topicString message: aByteArray qos: qos retain: retainFlag! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
readWaitTime: aSeconds
	self socketClient readWaitTime: aSeconds! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
restart
	(Delay forSeconds: 15) wait.
	self debugLog: 'MQTTClientInterface restarting possible existing session'.
	^ (self openOnHostName: self hostName) start! !

!MQTTClientInterface methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
socketClient
	^ socketClient! !

!MQTTClientInterface methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
socketClient: aValue
	socketClient := aValue! !

!MQTTClientInterface methodsFor: 'accessing' stamp: 'JMM 1/18/2026 12:00'!
socketClientClass
	^ MQTTSocketClient! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
start
	"Start actual socket connection"! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
stop
	self disconnect! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
subscribeTo: topicString qos: qos do: aBlock
	self socketClient onTopic: topicString qos: qos do: aBlock! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
unsubscribeFrom: aTopic
	self socketClient unsubscribeFrom: aTopic! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
username: uName password: pwd
	self socketClient username: uName password: pwd! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'JMM 1/18/2026 12:00'!
clientID: aString
	"Set a unique client ID. Must be called before connect."
	self socketClient clientID: aString! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"Extension methods"!

!True methodsFor: '*MQTT-Cuis' stamp: 'JMM 1/18/2026 12:00'!
asBit
	^ 1! !

!False methodsFor: '*MQTT-Cuis' stamp: 'JMM 1/18/2026 12:00'!
asBit
	^ 0! !

!Integer methodsFor: '*MQTT-Cuis' stamp: 'JMM 1/18/2026 12:00'!
bitAt: anInteger put: value
	"Set bit at position anInteger (1-based from right) to value (0 or 1)"
	value = 0
		ifTrue: [^ self bitAnd: (1 << (anInteger - 1)) bitInvert]
		ifFalse: [^ self bitOr: (1 << (anInteger - 1))]! !


"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

"Initialize the package"!

MQTTPacket initialize!

