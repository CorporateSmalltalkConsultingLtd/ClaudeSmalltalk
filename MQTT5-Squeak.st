'From Squeak 6.0 on 16 February 2026 at 2:58:00 am'!
'Description MQTT 5.0 Client Library for Squeak Smalltalk - Ported from Cuis'!

'Description MQTT 5.0 Client Library for Cuis Smalltalk - Upgraded from MQTT 3.1.1'!
SystemOrganization addCategory: #'MQTT5-Squeak'!
SystemOrganization addCategory: #'MQTT5-Squeak-IntegrationTests'!


WriteStream subclass: #MQTTWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTWriteStream class
	instanceVariableNames: ''!

Error subclass: #MQTT5ProtocolException
	instanceVariableNames: 'reasonCode reasonString'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTT5ProtocolException class
	instanceVariableNames: ''!

Error subclass: #MQTTBadHeaderException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTBadHeaderException class
	instanceVariableNames: ''!

Error subclass: #MQTTBadPacketTypeException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTBadPacketTypeException class
	instanceVariableNames: ''!

Error subclass: #MQTTBadTopicException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTBadTopicException class
	instanceVariableNames: ''!

Error subclass: #MQTTBrokerLookupFailure
	instanceVariableNames: 'brokerName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTBrokerLookupFailure class
	instanceVariableNames: ''!

Error subclass: #MQTTCONNECTException
	instanceVariableNames: 'connectReturnCode reasonCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTCONNECTException class
	instanceVariableNames: ''!

Error subclass: #MQTTConnectionException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTConnectionException class
	instanceVariableNames: ''!

Error subclass: #MQTTEmptyStreamErrorException
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTEmptyStreamErrorException class
	instanceVariableNames: ''!

Object subclass: #MQTT5Constants
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTT5Constants class
	instanceVariableNames: ''!

Object subclass: #MQTT5Properties
	instanceVariableNames: 'properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTT5Properties class
	instanceVariableNames: ''!

Object subclass: #MQTT5Property
	instanceVariableNames: 'identifier value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTT5Property class
	instanceVariableNames: ''!

Object subclass: #MQTT5ReasonCode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTT5ReasonCode class
	instanceVariableNames: ''!

Object subclass: #MQTT5ServerCapabilities
	instanceVariableNames: 'sessionExpiryInterval receiveMaximum maximumQoS retainAvailable maximumPacketSize assignedClientIdentifier topicAliasMaximum wildcardSubscriptionAvailable subscriptionIdentifiersAvailable sharedSubscriptionAvailable serverKeepAlive responseInformation serverReference authenticationMethod authenticationData'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTT5ServerCapabilities class
	instanceVariableNames: ''!

Object subclass: #MQTTClientInterface
	instanceVariableNames: 'socketClient hostName keepAliveTime protocolVersion'
	classVariableNames: 'TranscriptMutex'
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTClientInterface class
	instanceVariableNames: ''!

Object subclass: #MQTTPacket
	instanceVariableNames: ''
	classVariableNames: 'MQTTProtocolLevel311 MQTTProtocolLevel50 PacketTypes'
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacket class
	instanceVariableNames: ''!

MQTTPacket subclass: #MQTTPacketAuth
	instanceVariableNames: 'remainingLength reasonCode properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketAuth class
	instanceVariableNames: ''!

MQTTPacket subclass: #MQTTPacketDisconnect
	instanceVariableNames: 'reasonCode properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketDisconnect class
	instanceVariableNames: ''!

MQTTPacket subclass: #MQTTPacketPingReq
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketPingReq class
	instanceVariableNames: ''!

MQTTPacket subclass: #MQTTPacketPingResp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketPingResp class
	instanceVariableNames: ''!

MQTTPacket subclass: #MQTTPacketVariableHeaded
	instanceVariableNames: 'remainingLength msgID properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketVariableHeaded class
	instanceVariableNames: ''!

MQTTPacketVariableHeaded subclass: #MQTTPacketConnAck
	instanceVariableNames: 'sessionPresent byte1 byte2 reasonCode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketConnAck class
	instanceVariableNames: ''!

MQTTPacketVariableHeaded subclass: #MQTTPacketPubAck
	instanceVariableNames: 'reasonCode reasonString'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketPubAck class
	instanceVariableNames: ''!

MQTTPacketVariableHeaded subclass: #MQTTPacketPubComp
	instanceVariableNames: 'reasonCode reasonString'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketPubComp class
	instanceVariableNames: ''!

MQTTPacketVariableHeaded subclass: #MQTTPacketPubRec
	instanceVariableNames: 'originalPacket reasonCode reasonString'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketPubRec class
	instanceVariableNames: ''!

MQTTPacketVariableHeaded subclass: #MQTTPacketPubRel
	instanceVariableNames: 'reasonCode reasonString'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketPubRel class
	instanceVariableNames: ''!

MQTTPacketVariableHeaded subclass: #MQTTPacketUnsubAck
	instanceVariableNames: 'reasonCodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketUnsubAck class
	instanceVariableNames: ''!

MQTTPacketVariableHeaded subclass: #MQTTPacketVariableHeadedWithPayload
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketVariableHeadedWithPayload class
	instanceVariableNames: ''!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketConnect
	instanceVariableNames: 'user password keepAliveTime willMessage willTopic willQos willFlag willRetainFlag cleanSessionFlag clientID returnCode protocolVersion willProperties sessionExpiryInterval receiveMaximum maximumPacketSize topicAliasMaximum requestResponseInfo requestProblemInfo authenticationMethod authenticationData'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketConnect class
	instanceVariableNames: ''!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketPublish
	instanceVariableNames: 'topic message duplicate qos retain topicAlias payloadFormatIndicator messageExpiryInterval responseTopic correlationData subscriptionIdentifiers contentType'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketPublish class
	instanceVariableNames: ''!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketSubAck
	instanceVariableNames: 'returnCodes reasonCodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketSubAck class
	instanceVariableNames: ''!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketSubscribe
	instanceVariableNames: 'payloadDict subscriptionIdentifier'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketSubscribe class
	instanceVariableNames: ''!

MQTTPacketVariableHeadedWithPayload subclass: #MQTTPacketUnsubscribe
	instanceVariableNames: 'topics'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketUnsubscribe class
	instanceVariableNames: ''!

Object subclass: #MQTTPacketAndPendingJobPair
	instanceVariableNames: 'packet pendingJob'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPacketAndPendingJobPair class
	instanceVariableNames: ''!

Object subclass: #MQTTPendingJob
	instanceVariableNames: 'originalPacket sendTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPendingJob class
	instanceVariableNames: ''!

MQTTPendingJob subclass: #MQTTPendingPingJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPendingPingJob class
	instanceVariableNames: ''!

MQTTPendingJob subclass: #MQTTPendingPubAckJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPendingPubAckJob class
	instanceVariableNames: ''!

MQTTPendingJob subclass: #MQTTPendingPubCompJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPendingPubCompJob class
	instanceVariableNames: ''!

MQTTPendingJob subclass: #MQTTPendingPubRecJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPendingPubRecJob class
	instanceVariableNames: ''!

MQTTPendingJob subclass: #MQTTPendingPubRelJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPendingPubRelJob class
	instanceVariableNames: ''!

MQTTPendingJob subclass: #MQTTPendingSubAckJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPendingSubAckJob class
	instanceVariableNames: ''!

MQTTPendingJob subclass: #MQTTPendingUnsubAckJob
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTPendingUnsubAckJob class
	instanceVariableNames: ''!

Object subclass: #MQTTSocketClient
	instanceVariableNames: 'transport cleanSessionFlag willTopic willMessage willRetainFlag willQos userNameString passwordString keepAliveTime socket interface pendingSocket pendingClientID protocolVersion'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTSocketClient class
	instanceVariableNames: ''!

Object subclass: #MQTTStatistics
	instanceVariableNames: 'pin pout poutMaxSize poutMinSize pinMaxSize pinMinSize pinBytes poutBytes pinTypeCount poutTypeCount errorTypes connections disconnect connected connectionTypes servers subscriptions timeOut killSocket sentPackets queuedPackets unsubscribe badConnections missingType resendType'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTStatistics class
	instanceVariableNames: ''!

Object subclass: #MQTTSubscription
	instanceVariableNames: 'topic actionBlock qos retainFlag noLocal retainAsPublished retainHandling'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTSubscription class
	instanceVariableNames: ''!

Object subclass: #MQTTTransportLayer
	instanceVariableNames: 'sockStrm abort incomingPacketProcess packetInFlightQueue outgoingPacketQueue outgoingPacketProcess retryTime pendingJobsMutex abortMutex retryProcess lastMID pendingJobs currentSubscriptions readWaitTime statisticsMutex statistics protocolVersion'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTTransportLayer class
	instanceVariableNames: ''!

MQTTTransportLayer subclass: #MQTTTransportLayerClient
	instanceVariableNames: 'pingOK keepAliveTime clientID socketClient serverCapabilities topicAliasMapOutgoing topicAliasMapIncoming'
	classVariableNames: 'ClientID'
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
MQTTTransportLayerClient class
	instanceVariableNames: ''!

Object subclass: #UTF8Encoder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!
UTF8Encoder class
	instanceVariableNames: ''!

TestCase subclass: #MQTT5IntegrationTest
	instanceVariableNames: 'client'
	classVariableNames: 'BrokerHost BrokerPassword BrokerPort BrokerUsername'
	poolDictionaries: ''
	category: 'MQTT5-Squeak-IntegrationTests'!
MQTT5IntegrationTest class
	instanceVariableNames: ''!

MQTT5IntegrationTest subclass: #MQTT5ConnectionTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-IntegrationTests'!
MQTT5ConnectionTest class
	instanceVariableNames: ''!

MQTT5IntegrationTest subclass: #MQTT5PropertiesIntegrationTest
	instanceVariableNames: 'receivedProperties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-IntegrationTests'!
MQTT5PropertiesIntegrationTest class
	instanceVariableNames: ''!

MQTT5IntegrationTest subclass: #MQTT5PublishSubscribeTest
	instanceVariableNames: 'received receivedTopic'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak-IntegrationTests'!
MQTT5PublishSubscribeTest class
	instanceVariableNames: ''!


!MQTTWriteStream methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.! !

!MQTTWriteStream class methodsFor: 'instance creation' stamp: ''!
on: arg1
	^ super on: arg1.! !

!MQTT5ProtocolException methodsFor: 'accessing' stamp: ''!
messageText
	^ reasonString ifNil: [ 'MQTT Protocol Error: ' , (MQTT5ReasonCode descriptionFor: reasonCode) ].! !

!MQTT5ProtocolException methodsFor: 'accessing' stamp: ''!
reasonCode
	^ reasonCode.! !

!MQTT5ProtocolException methodsFor: 'accessing' stamp: ''!
reasonCode: arg1
	reasonCode := arg1.! !

!MQTT5ProtocolException methodsFor: 'accessing' stamp: ''!
reasonString
	^ reasonString.! !

!MQTT5ProtocolException methodsFor: 'accessing' stamp: ''!
reasonString: arg1
	reasonString := arg1.! !

!MQTT5ProtocolException methodsFor: 'printing' stamp: ''!
printOn: arg1
	super printOn: arg1.
	arg1 nextPutAll: ' reasonCode: '.
	reasonCode printOn: arg1.
	arg1 nextPutAll: ' ('.
	arg1 nextPutAll: (MQTT5Constants descriptionForReasonCode: reasonCode).
	arg1 nextPutAll: ')'.
	reasonString ifNotNil: [
		arg1 nextPutAll: ' reasonString: '.
		reasonString printOn: arg1 ].! !

!MQTT5ProtocolException methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCode := 0.! !

!MQTT5ProtocolException class methodsFor: 'instance creation' stamp: ''!
reasonCode: arg1
	^ self new
		reasonCode: arg1;
		yourself.! !

!MQTT5ProtocolException class methodsFor: 'instance creation' stamp: ''!
reasonCode: arg1 reasonString: arg2
	^ self new
		reasonCode: arg1;
		reasonString: arg2;
		yourself.! !

!MQTTBrokerLookupFailure methodsFor: 'accessing' stamp: ''!
brokerName
	^ brokerName.! !

!MQTTBrokerLookupFailure methodsFor: 'accessing' stamp: ''!
brokerName: arg1
	brokerName := arg1.! !

!MQTTBrokerLookupFailure methodsFor: 'accessing' stamp: ''!
messageText
	^ 'Failed to resolve broker: ' , (brokerName ifNil: [ '(nil)' ]).! !

!MQTTBrokerLookupFailure class methodsFor: 'instance creation' stamp: ''!
brokerName: arg1
	^ self new brokerName: arg1.! !

!MQTTCONNECTException methodsFor: 'accessing' stamp: ''!
connectReturnCode
	^ connectReturnCode.! !

!MQTTCONNECTException methodsFor: 'accessing' stamp: ''!
connectReturnCode: arg1
	connectReturnCode := arg1.! !

!MQTTCONNECTException methodsFor: 'accessing' stamp: ''!
messageText
	reasonCode > 0 ifTrue: [ ^ 'MQTT Connection Failed: ' , (MQTT5ReasonCode descriptionFor: reasonCode) ].
	connectReturnCode = 1 ifTrue: [ ^ 'Connection Refused: unacceptable protocol version' ].
	connectReturnCode = 2 ifTrue: [ ^ 'Connection Refused: identifier rejected' ].
	connectReturnCode = 3 ifTrue: [ ^ 'Connection Refused: server unavailable' ].
	connectReturnCode = 4 ifTrue: [ ^ 'Connection Refused: bad user name or password' ].
	connectReturnCode = 5 ifTrue: [ ^ 'Connection Refused: not authorized' ].
	^ 'Connection Failed: unknown error code ' , connectReturnCode printString.! !

!MQTTCONNECTException methodsFor: 'accessing' stamp: ''!
reasonCode
	^ reasonCode.! !

!MQTTCONNECTException methodsFor: 'accessing' stamp: ''!
reasonCode: arg1
	reasonCode := arg1.! !

!MQTTCONNECTException methodsFor: 'printing' stamp: ''!
printOn: arg1
	super printOn: arg1.
	connectReturnCode ifNotNil: [
		arg1 nextPutAll: ' returnCode: '.
		connectReturnCode printOn: arg1 ].
	reasonCode ifNotNil: [
		arg1 nextPutAll: ' reasonCode: '.
		reasonCode printOn: arg1.
		arg1 nextPutAll: ' ('.
		arg1 nextPutAll: (MQTT5Constants descriptionForReasonCode: reasonCode).
		arg1 nextPutAll: ')' ].! !

!MQTTCONNECTException methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	connectReturnCode := 0.
	reasonCode := 0.! !

!MQTTCONNECTException class methodsFor: 'instance creation' stamp: ''!
connectReturnCode: arg1
	^ (self new connectReturnCode: arg1) signal.! !

!MQTTCONNECTException class methodsFor: 'instance creation' stamp: ''!
reasonCode: arg1
	^ (self new reasonCode: arg1) signal.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
assignedClientIdentifier
	^ 18.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
authenticationData
	^ 22.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
authenticationMethod
	^ 21.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
contentType
	^ 3.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
correlationData
	^ 9.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
maximumPacketSize
	^ 39.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
maximumQoS
	^ 36.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
messageExpiryInterval
	^ 2.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
payloadFormatIndicator
	^ 1.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
reasonString
	^ 31.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
receiveMaximum
	^ 33.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
requestProblemInformation
	^ 23.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
requestResponseInformation
	^ 25.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
responseInformation
	^ 26.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
responseTopic
	^ 8.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
retainAvailable
	^ 37.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
serverKeepAlive
	^ 19.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
serverReference
	^ 28.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
sessionExpiryInterval
	^ 17.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
sharedSubscriptionAvailable
	^ 42.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
subscriptionIdentifier
	^ 11.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
subscriptionIdentifiersAvailable
	^ 41.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
topicAlias
	^ 35.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
topicAliasMaximum
	^ 34.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
userProperty
	^ 38.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
wildcardSubscriptionAvailable
	^ 40.! !

!MQTT5Constants class methodsFor: 'property identifiers' stamp: ''!
willDelayInterval
	^ 24.! !

!MQTT5Constants class methodsFor: 'property type info' stamp: ''!
propertyTypeFor: arg1
	arg1 = 1 ifTrue: [ ^ #byte ].
	arg1 = 2 ifTrue: [ ^ #fourByteInteger ].
	arg1 = 3 ifTrue: [ ^ #utf8String ].
	arg1 = 8 ifTrue: [ ^ #utf8String ].
	arg1 = 9 ifTrue: [ ^ #binaryData ].
	arg1 = 11 ifTrue: [ ^ #variableByteInteger ].
	arg1 = 17 ifTrue: [ ^ #fourByteInteger ].
	arg1 = 18 ifTrue: [ ^ #utf8String ].
	arg1 = 19 ifTrue: [ ^ #twoByteInteger ].
	arg1 = 21 ifTrue: [ ^ #utf8String ].
	arg1 = 22 ifTrue: [ ^ #binaryData ].
	arg1 = 23 ifTrue: [ ^ #byte ].
	arg1 = 24 ifTrue: [ ^ #fourByteInteger ].
	arg1 = 25 ifTrue: [ ^ #byte ].
	arg1 = 26 ifTrue: [ ^ #utf8String ].
	arg1 = 28 ifTrue: [ ^ #utf8String ].
	arg1 = 31 ifTrue: [ ^ #utf8String ].
	arg1 = 33 ifTrue: [ ^ #twoByteInteger ].
	arg1 = 34 ifTrue: [ ^ #twoByteInteger ].
	arg1 = 35 ifTrue: [ ^ #twoByteInteger ].
	arg1 = 36 ifTrue: [ ^ #byte ].
	arg1 = 37 ifTrue: [ ^ #byte ].
	arg1 = 38 ifTrue: [ ^ #utf8StringPair ].
	arg1 = 39 ifTrue: [ ^ #fourByteInteger ].
	arg1 = 40 ifTrue: [ ^ #byte ].
	arg1 = 41 ifTrue: [ ^ #byte ].
	arg1 = 42 ifTrue: [ ^ #byte ].
	^ nil.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
descriptionForReasonCode: arg1
	arg1 = 0 ifTrue: [ ^ 'Success' ].
	arg1 = 1 ifTrue: [ ^ 'Granted QoS 1' ].
	arg1 = 2 ifTrue: [ ^ 'Granted QoS 2' ].
	arg1 = 4 ifTrue: [ ^ 'Disconnect with Will Message' ].
	arg1 = 16 ifTrue: [ ^ 'No matching subscribers' ].
	arg1 = 17 ifTrue: [ ^ 'No subscription existed' ].
	arg1 = 24 ifTrue: [ ^ 'Continue authentication' ].
	arg1 = 25 ifTrue: [ ^ 'Re-authenticate' ].
	arg1 = 128 ifTrue: [ ^ 'Unspecified error' ].
	arg1 = 129 ifTrue: [ ^ 'Malformed Packet' ].
	arg1 = 130 ifTrue: [ ^ 'Protocol Error' ].
	arg1 = 131 ifTrue: [ ^ 'Implementation specific error' ].
	arg1 = 132 ifTrue: [ ^ 'Unsupported Protocol Version' ].
	arg1 = 133 ifTrue: [ ^ 'Client Identifier not valid' ].
	arg1 = 134 ifTrue: [ ^ 'Bad User Name or Password' ].
	arg1 = 135 ifTrue: [ ^ 'Not authorized' ].
	arg1 = 136 ifTrue: [ ^ 'Server unavailable' ].
	arg1 = 137 ifTrue: [ ^ 'Server busy' ].
	arg1 = 138 ifTrue: [ ^ 'Banned' ].
	arg1 = 139 ifTrue: [ ^ 'Server shutting down' ].
	arg1 = 140 ifTrue: [ ^ 'Bad authentication method' ].
	arg1 = 141 ifTrue: [ ^ 'Keep Alive timeout' ].
	arg1 = 142 ifTrue: [ ^ 'Session taken over' ].
	arg1 = 143 ifTrue: [ ^ 'Topic Filter invalid' ].
	arg1 = 144 ifTrue: [ ^ 'Topic Name invalid' ].
	arg1 = 145 ifTrue: [ ^ 'Packet Identifier in use' ].
	arg1 = 146 ifTrue: [ ^ 'Packet Identifier not found' ].
	arg1 = 147 ifTrue: [ ^ 'Receive Maximum exceeded' ].
	arg1 = 148 ifTrue: [ ^ 'Topic Alias invalid' ].
	arg1 = 149 ifTrue: [ ^ 'Packet too large' ].
	arg1 = 150 ifTrue: [ ^ 'Message rate too high' ].
	arg1 = 151 ifTrue: [ ^ 'Quota exceeded' ].
	arg1 = 152 ifTrue: [ ^ 'Administrative action' ].
	arg1 = 153 ifTrue: [ ^ 'Payload format invalid' ].
	arg1 = 154 ifTrue: [ ^ 'Retain not supported' ].
	arg1 = 155 ifTrue: [ ^ 'QoS not supported' ].
	arg1 = 156 ifTrue: [ ^ 'Use another server' ].
	arg1 = 157 ifTrue: [ ^ 'Server moved' ].
	arg1 = 158 ifTrue: [ ^ 'Shared Subscriptions not supported' ].
	arg1 = 159 ifTrue: [ ^ 'Connection rate exceeded' ].
	arg1 = 160 ifTrue: [ ^ 'Maximum connect time' ].
	arg1 = 161 ifTrue: [ ^ 'Subscription Identifiers not supported' ].
	arg1 = 162 ifTrue: [ ^ 'Wildcard Subscriptions not supported' ].
	^ 'Unknown reason code: ' , arg1 printString.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
isErrorReasonCode: arg1
	^ arg1 >= 128.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeAdministrativeAction
	^ 152.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeBadAuthenticationMethod
	^ 140.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeBadUserNameOrPassword
	^ 134.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeBanned
	^ 138.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeClientIdentifierNotValid
	^ 133.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeConnectionRateExceeded
	^ 159.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeContinueAuthentication
	^ 24.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeDisconnectWithWillMessage
	^ 4.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeGrantedQoS0
	^ 0.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeGrantedQoS1
	^ 1.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeGrantedQoS2
	^ 2.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeImplementationSpecificError
	^ 131.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeKeepAliveTimeout
	^ 141.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeMalformedPacket
	^ 129.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeMaximumConnectTime
	^ 160.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeMessageRateTooHigh
	^ 150.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeNoMatchingSubscribers
	^ 16.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeNoSubscriptionExisted
	^ 17.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeNormalDisconnection
	^ 0.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeNotAuthorized
	^ 135.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodePacketIdentifierInUse
	^ 145.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodePacketIdentifierNotFound
	^ 146.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodePacketTooLarge
	^ 149.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodePayloadFormatInvalid
	^ 153.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeProtocolError
	^ 130.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeQoSNotSupported
	^ 155.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeQuotaExceeded
	^ 151.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeReAuthenticate
	^ 25.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeReceiveMaximumExceeded
	^ 147.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeRetainNotSupported
	^ 154.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeServerBusy
	^ 137.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeServerMoved
	^ 157.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeServerShuttingDown
	^ 139.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeServerUnavailable
	^ 136.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeSessionTakenOver
	^ 142.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeSharedSubscriptionsNotSupported
	^ 158.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeSubscriptionIdentifiersNotSupported
	^ 161.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeSuccess
	^ 0.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeTopicAliasInvalid
	^ 148.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeTopicFilterInvalid
	^ 143.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeTopicNameInvalid
	^ 144.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeUnspecifiedError
	^ 128.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeUnsupportedProtocolVersion
	^ 132.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeUseAnotherServer
	^ 156.! !

!MQTT5Constants class methodsFor: 'reason codes' stamp: ''!
reasonCodeWildcardSubscriptionsNotSupported
	^ 162.! !

!MQTT5Properties methodsFor: 'initialization' stamp: ''!
initialize
	properties := OrderedCollection new.! !

!MQTT5Properties methodsFor: 'accessing' stamp: ''!
add: arg1
	self addProperty: arg1.! !

!MQTT5Properties methodsFor: 'accessing' stamp: ''!
addProperty: arg1
	properties add: arg1.! !

!MQTT5Properties methodsFor: 'accessing' stamp: ''!
addPropertyIdentifier: arg1 value: arg2
	self addProperty:
		(MQTT5Property
			identifier: arg1
			value: arg2).! !

!MQTT5Properties methodsFor: 'accessing' stamp: ''!
properties
	^ properties.! !

!MQTT5Properties methodsFor: 'accessing' stamp: ''!
propertiesAt: arg1
	^ (properties select: [ :argm0_2 |
		argm0_2 identifier = arg1 ]) collect: [ :argm0_2 |
		argm0_2 value ].! !

!MQTT5Properties methodsFor: 'accessing' stamp: ''!
propertyAt: arg1
	properties do: [ :argm0_2 |
		argm0_2 identifier = arg1 ifTrue: [ ^ argm0_2 value ]].
	^ nil.! !

!MQTT5Properties methodsFor: 'accessing' stamp: ''!
size
	^ properties size.! !

!MQTT5Properties methodsFor: 'testing' stamp: ''!
isEmpty
	^ properties isEmpty.! !

!MQTT5Properties methodsFor: 'testing' stamp: ''!
notEmpty
	^ properties notEmpty.! !

!MQTT5Properties methodsFor: 'encoding' stamp: ''!
encodeOn: arg1
	| temp2 |
	properties isEmpty ifTrue: [
		arg1 nextPut: 0.
		^ self ].
	temp2 := MQTTWriteStream on: (ByteArray new: self estimatedSize).
	properties do: [ :argm2_3 |
		argm2_3 encodeOn: temp2 ].
	self
		encodeVariableByteInteger: temp2 position
		on: arg1.
	arg1 nextPutAll: temp2 contents.! !

!MQTT5Properties methodsFor: 'encoding' stamp: ''!
encodeVariableByteInteger: arg1 on: arg2
	| temp3 temp4 |
	temp3 := arg1.
	[
	temp4 := temp3 \\ 128.
	temp3 := temp3 // 128.
	temp3 > 0 ifTrue: [ temp4 := temp4 bitOr: 128 ].
	arg2 nextPut: temp4.
	temp3 > 0 ] whileTrue.! !

!MQTT5Properties methodsFor: 'encoding' stamp: ''!
estimatedSize
	| temp1 |
	temp1 := 0.
	properties do: [ :argm1_3 |
		temp1 := temp1 + argm1_3 encodedSize ].
	^ temp1 + 4.! !

!MQTT5Properties methodsFor: 'decoding' stamp: ''!
decode16BitIntegerFrom: arg1
	^ arg1 next << 8 + arg1 next.! !

!MQTT5Properties methodsFor: 'decoding' stamp: ''!
decode32BitIntegerFrom: arg1
	^ arg1 next << 24 + (arg1 next << 16) + (arg1 next << 8) + arg1 next.! !

!MQTT5Properties methodsFor: 'decoding' stamp: ''!
decodeBinaryDataFrom: arg1
	| temp2 |
	temp2 := self decode16BitIntegerFrom: arg1.
	^ arg1 next: temp2.! !

!MQTT5Properties methodsFor: 'decoding' stamp: ''!
decodeFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 next: temp2.
	temp4 := ReadStream on: temp3.
	[ temp4 atEnd ] whileFalse: [ self decodePropertyFrom: temp4 ].! !

!MQTT5Properties methodsFor: 'decoding' stamp: ''!
decodePropertyFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp3 := MQTT5Constants propertyTypeFor: temp2.
	temp3 = #byte ifTrue: [ temp4 := arg1 next ].
	temp3 = #twoByteInteger ifTrue: [ temp4 := self decode16BitIntegerFrom: arg1 ].
	temp3 = #fourByteInteger ifTrue: [ temp4 := self decode32BitIntegerFrom: arg1 ].
	temp3 = #variableByteInteger ifTrue: [ temp4 := self decodeVariableByteIntegerFrom: arg1 ].
	temp3 = #utf8String ifTrue: [ temp4 := self decodeStringFrom: arg1 ].
	temp3 = #binaryData ifTrue: [ temp4 := self decodeBinaryDataFrom: arg1 ].
	temp3 = #utf8StringPair ifTrue: [ temp4 := (self decodeStringFrom: arg1) -> (self decodeStringFrom: arg1) ].
	temp3 ifNil: [ self error: 'Unknown property identifier: ' , temp2 printString ].
	properties add:
		(MQTT5Property
			identifier: temp2
			value: temp4).! !

!MQTT5Properties methodsFor: 'decoding' stamp: ''!
decodeStringFrom: arg1
	| temp2 temp3 |
	temp2 := self decode16BitIntegerFrom: arg1.
	temp3 := arg1 next: temp2.
	^ UTF8Encoder decode: temp3.! !

!MQTT5Properties methodsFor: 'decoding' stamp: ''!
decodeVariableByteIntegerFrom: arg1
	| temp2 temp3 temp4 |
	temp3 := 0.
	temp4 := 0.
	[
	temp2 := arg1 next.
	(temp2 isNil or: [ temp3 > 21 ]) ifTrue: [ self error: 'Invalid variable byte integer' ].
	temp4 := temp4 + ((temp2 bitAnd: 127) << temp3).
	temp3 := temp3 + 7.
	(temp2 bitAnd: 128) > 0 ] whileTrue.
	^ temp4.! !

!MQTT5Properties methodsFor: 'enumerating' stamp: ''!
do: arg1
	properties do: arg1.! !

!MQTT5Properties methodsFor: 'printing' stamp: ''!
printOn: arg1
	super printOn: arg1.
	arg1 nextPutAll: ' (' , properties size printString , ' properties)'.! !

!MQTT5Properties class methodsFor: 'instance creation' stamp: ''!
new
	^ super new initialize.! !

!MQTT5Property methodsFor: 'accessing' stamp: ''!
identifier
	^ identifier.! !

!MQTT5Property methodsFor: 'accessing' stamp: ''!
identifier: arg1 value: arg2
	identifier := arg1.
	value := arg2.! !

!MQTT5Property methodsFor: 'accessing' stamp: ''!
value
	^ value.! !

!MQTT5Property methodsFor: 'accessing' stamp: ''!
value: arg1
	value := arg1.! !

!MQTT5Property methodsFor: 'encoding' stamp: ''!
encode16BitInteger: arg1 on: arg2
	arg2
		nextPut: (arg1 >> 8 bitAnd: 255);
		nextPut: (arg1 bitAnd: 255).! !

!MQTT5Property methodsFor: 'encoding' stamp: ''!
encode32BitInteger: arg1 on: arg2
	arg2
		nextPut: (arg1 >> 24 bitAnd: 255);
		nextPut: (arg1 >> 16 bitAnd: 255);
		nextPut: (arg1 >> 8 bitAnd: 255);
		nextPut: (arg1 bitAnd: 255).! !

!MQTT5Property methodsFor: 'encoding' stamp: ''!
encodeBinaryData: arg1 on: arg2
	self
		encode16BitInteger: arg1 size
		on: arg2.
	arg2 nextPutAll: arg1.! !

!MQTT5Property methodsFor: 'encoding' stamp: ''!
encodeOn: arg1
	| temp2 |
	self
		encodeVariableByteInteger: identifier
		on: arg1.
	temp2 := MQTT5Constants propertyTypeFor: identifier.
	temp2 = #byte ifTrue: [ ^ arg1 nextPut: value ].
	temp2 = #twoByteInteger ifTrue: [ ^ self
			encode16BitInteger: value
			on: arg1 ].
	temp2 = #fourByteInteger ifTrue: [ ^ self
			encode32BitInteger: value
			on: arg1 ].
	temp2 = #variableByteInteger ifTrue: [ ^ self
			encodeVariableByteInteger: value
			on: arg1 ].
	temp2 = #utf8String ifTrue: [ ^ self
			encodeString: value
			on: arg1 ].
	temp2 = #binaryData ifTrue: [ ^ self
			encodeBinaryData: value
			on: arg1 ].
	temp2 = #utf8StringPair ifTrue: [
		self
			encodeString: value key
			on: arg1.
		^ self
			encodeString: value value
			on: arg1 ].
	self error: 'Unknown property type for identifier: ' , identifier printString.! !

!MQTT5Property methodsFor: 'encoding' stamp: ''!
encodeString: arg1 on: arg2
	| temp3 |
	temp3 := UTF8Encoder encode: arg1 asString.
	self
		encode16BitInteger: temp3 size
		on: arg2.
	arg2 nextPutAll: temp3.! !

!MQTT5Property methodsFor: 'encoding' stamp: ''!
encodeVariableByteInteger: arg1 on: arg2
	| temp3 temp4 |
	temp3 := arg1.
	[
	temp4 := temp3 \\ 128.
	temp3 := temp3 // 128.
	temp3 > 0 ifTrue: [ temp4 := temp4 bitOr: 128 ].
	arg2 nextPut: temp4.
	temp3 > 0 ] whileTrue.! !

!MQTT5Property methodsFor: 'encoding' stamp: ''!
encodedSize
	| temp1 temp2 |
	temp2 := self variableByteIntegerSize: identifier.
	temp1 := MQTT5Constants propertyTypeFor: identifier.
	temp1 = #byte ifTrue: [ ^ temp2 + 1 ].
	temp1 = #twoByteInteger ifTrue: [ ^ temp2 + 2 ].
	temp1 = #fourByteInteger ifTrue: [ ^ temp2 + 4 ].
	temp1 = #variableByteInteger ifTrue: [ ^ temp2 + (self variableByteIntegerSize: value) ].
	temp1 = #utf8String ifTrue: [ ^ temp2 + 2 + (UTF8Encoder encode: value asString) size ].
	temp1 = #binaryData ifTrue: [ ^ temp2 + 2 + value size ].
	temp1 = #utf8StringPair ifTrue: [ ^ temp2 + 2 + (UTF8Encoder encode: value key asString) size + 2 + (UTF8Encoder encode: value value asString) size ].
	^ temp2.! !

!MQTT5Property methodsFor: 'encoding' stamp: ''!
variableByteIntegerSize: arg1
	arg1 < 128 ifTrue: [ ^ 1 ].
	arg1 < 16384 ifTrue: [ ^ 2 ].
	arg1 < 2097152 ifTrue: [ ^ 3 ].
	^ 4.! !

!MQTT5Property methodsFor: 'printing' stamp: ''!
printOn: arg1
	super printOn: arg1.
	arg1 nextPutAll: ' id: '.
	identifier printOn: arg1.
	arg1 nextPutAll: ' value: '.
	value printOn: arg1.! !

!MQTT5Property class methodsFor: 'instance creation' stamp: ''!
identifier: arg1 value: arg2
	^ self new
		identifier: arg1
		value: arg2.! !

!MQTT5ReasonCode class methodsFor: 'testing' stamp: ''!
isError: arg1
	^ arg1 >= 128.! !

!MQTT5ReasonCode class methodsFor: 'testing' stamp: ''!
isSuccess: arg1
	^ arg1 <= 2.! !

!MQTT5ReasonCode class methodsFor: 'accessing' stamp: ''!
descriptionFor: arg1
	arg1 = 0 ifTrue: [ ^ 'Success' ].
	arg1 = 1 ifTrue: [ ^ 'Granted QoS 1' ].
	arg1 = 2 ifTrue: [ ^ 'Granted QoS 2' ].
	arg1 = 4 ifTrue: [ ^ 'Disconnect with Will Message' ].
	arg1 = 16 ifTrue: [ ^ 'No matching subscribers' ].
	arg1 = 17 ifTrue: [ ^ 'No subscription existed' ].
	arg1 = 24 ifTrue: [ ^ 'Continue authentication' ].
	arg1 = 25 ifTrue: [ ^ 'Re-authenticate' ].
	arg1 = 128 ifTrue: [ ^ 'Unspecified error' ].
	arg1 = 129 ifTrue: [ ^ 'Malformed Packet' ].
	arg1 = 130 ifTrue: [ ^ 'Protocol Error' ].
	arg1 = 131 ifTrue: [ ^ 'Implementation specific error' ].
	arg1 = 132 ifTrue: [ ^ 'Unsupported Protocol Version' ].
	arg1 = 133 ifTrue: [ ^ 'Client Identifier not valid' ].
	arg1 = 134 ifTrue: [ ^ 'Bad User Name or Password' ].
	arg1 = 135 ifTrue: [ ^ 'Not authorized' ].
	arg1 = 136 ifTrue: [ ^ 'Server unavailable' ].
	arg1 = 137 ifTrue: [ ^ 'Server busy' ].
	arg1 = 138 ifTrue: [ ^ 'Banned' ].
	arg1 = 139 ifTrue: [ ^ 'Server shutting down' ].
	arg1 = 140 ifTrue: [ ^ 'Bad authentication method' ].
	arg1 = 141 ifTrue: [ ^ 'Keep Alive timeout' ].
	arg1 = 142 ifTrue: [ ^ 'Session taken over' ].
	arg1 = 143 ifTrue: [ ^ 'Topic Filter invalid' ].
	arg1 = 144 ifTrue: [ ^ 'Topic Name invalid' ].
	arg1 = 145 ifTrue: [ ^ 'Packet Identifier in use' ].
	arg1 = 146 ifTrue: [ ^ 'Packet Identifier not found' ].
	arg1 = 147 ifTrue: [ ^ 'Receive Maximum exceeded' ].
	arg1 = 148 ifTrue: [ ^ 'Topic Alias invalid' ].
	arg1 = 149 ifTrue: [ ^ 'Packet too large' ].
	arg1 = 150 ifTrue: [ ^ 'Message rate too high' ].
	arg1 = 151 ifTrue: [ ^ 'Quota exceeded' ].
	arg1 = 152 ifTrue: [ ^ 'Administrative action' ].
	arg1 = 153 ifTrue: [ ^ 'Payload format invalid' ].
	arg1 = 154 ifTrue: [ ^ 'Retain not supported' ].
	arg1 = 155 ifTrue: [ ^ 'QoS not supported' ].
	arg1 = 156 ifTrue: [ ^ 'Use another server' ].
	arg1 = 157 ifTrue: [ ^ 'Server moved' ].
	arg1 = 158 ifTrue: [ ^ 'Shared Subscriptions not supported' ].
	arg1 = 159 ifTrue: [ ^ 'Connection rate exceeded' ].
	arg1 = 160 ifTrue: [ ^ 'Maximum connect time' ].
	arg1 = 161 ifTrue: [ ^ 'Subscription Identifiers not supported' ].
	arg1 = 162 ifTrue: [ ^ 'Wildcard Subscriptions not supported' ].
	^ 'Unknown reason code: ' , arg1 printString.! !

!MQTT5ServerCapabilities methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	receiveMaximum := 65535.
	maximumQoS := 2.
	retainAvailable := true.
	maximumPacketSize := 268435456.
	topicAliasMaximum := 0.
	wildcardSubscriptionAvailable := true.
	subscriptionIdentifiersAvailable := true.
	sharedSubscriptionAvailable := true.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
assignedClientIdentifier
	^ assignedClientIdentifier.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
assignedClientIdentifier: arg1
	assignedClientIdentifier := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
authenticationData
	^ authenticationData.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
authenticationData: arg1
	authenticationData := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
authenticationMethod
	^ authenticationMethod.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
authenticationMethod: arg1
	authenticationMethod := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
maximumPacketSize
	^ maximumPacketSize.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
maximumPacketSize: arg1
	maximumPacketSize := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
maximumQoS
	^ maximumQoS.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
maximumQoS: arg1
	maximumQoS := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
receiveMaximum
	^ receiveMaximum.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
receiveMaximum: arg1
	receiveMaximum := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
responseInformation
	^ responseInformation.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
responseInformation: arg1
	responseInformation := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
retainAvailable
	^ retainAvailable.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
retainAvailable: arg1
	retainAvailable := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
serverKeepAlive
	^ serverKeepAlive.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
serverKeepAlive: arg1
	serverKeepAlive := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
serverReference
	^ serverReference.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
serverReference: arg1
	serverReference := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
sessionExpiryInterval
	^ sessionExpiryInterval.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
sessionExpiryInterval: arg1
	sessionExpiryInterval := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
sharedSubscriptionAvailable
	^ sharedSubscriptionAvailable.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
sharedSubscriptionAvailable: arg1
	sharedSubscriptionAvailable := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
subscriptionIdentifiersAvailable
	^ subscriptionIdentifiersAvailable.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
subscriptionIdentifiersAvailable: arg1
	subscriptionIdentifiersAvailable := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
topicAliasMaximum
	^ topicAliasMaximum.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
topicAliasMaximum: arg1
	topicAliasMaximum := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
wildcardSubscriptionAvailable
	^ wildcardSubscriptionAvailable.! !

!MQTT5ServerCapabilities methodsFor: 'accessing' stamp: ''!
wildcardSubscriptionAvailable: arg1
	wildcardSubscriptionAvailable := arg1.! !

!MQTT5ServerCapabilities methodsFor: 'parsing' stamp: ''!
updateFromConnAckProperties: arg1
	arg1 ifNil: [ ^ self ].
	arg1 do: [ :argm1_2 |
		argm1_2 identifier = 17 ifTrue: [ sessionExpiryInterval := argm1_2 value ].
		argm1_2 identifier = 33 ifTrue: [ receiveMaximum := argm1_2 value ].
		argm1_2 identifier = 36 ifTrue: [ maximumQoS := argm1_2 value ].
		argm1_2 identifier = 37 ifTrue: [ retainAvailable := argm1_2 value = 1 ].
		argm1_2 identifier = 39 ifTrue: [ maximumPacketSize := argm1_2 value ].
		argm1_2 identifier = 18 ifTrue: [ assignedClientIdentifier := argm1_2 value ].
		argm1_2 identifier = 34 ifTrue: [ topicAliasMaximum := argm1_2 value ].
		argm1_2 identifier = 40 ifTrue: [ wildcardSubscriptionAvailable := argm1_2 value = 1 ].
		argm1_2 identifier = 41 ifTrue: [ subscriptionIdentifiersAvailable := argm1_2 value = 1 ].
		argm1_2 identifier = 42 ifTrue: [ sharedSubscriptionAvailable := argm1_2 value = 1 ].
		argm1_2 identifier = 19 ifTrue: [ serverKeepAlive := argm1_2 value ].
		argm1_2 identifier = 26 ifTrue: [ responseInformation := argm1_2 value ].
		argm1_2 identifier = 28 ifTrue: [ serverReference := argm1_2 value ].
		argm1_2 identifier = 21 ifTrue: [ authenticationMethod := argm1_2 value ].
		argm1_2 identifier = 22 ifTrue: [ authenticationData := argm1_2 value ]].! !

!MQTT5ServerCapabilities class methodsFor: 'instance creation' stamp: ''!
new
	^ super new initialize.! !

!MQTTClientInterface methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	keepAliveTime := 60.
	protocolVersion := 5.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
hostName
	^ hostName.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
hostName: arg1
	hostName := arg1.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
keepAliveTime
	^ keepAliveTime.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
keepAliveTime: arg1
	keepAliveTime := arg1.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
protocolVersion
	^ protocolVersion.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
protocolVersion: arg1
	protocolVersion := arg1.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
socketClient
	^ socketClient.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
socketClient: arg1
	socketClient := arg1.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
socketClientClass
	^ MQTTSocketClient.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
useVersion311
	protocolVersion := 4.! !

!MQTTClientInterface methodsFor: 'accessing' stamp: ''!
useVersion50
	protocolVersion := 5.! !

!MQTTClientInterface methodsFor: 'testing' stamp: ''!
isConnected
	^ self isValid.! !

!MQTTClientInterface methodsFor: 'testing' stamp: ''!
isValid
	^ (socketClient notNil and: [ socketClient transport notNil ]) and: [ socketClient transport abort not ].! !

!MQTTClientInterface methodsFor: 'testing' stamp: ''!
isVersion5
	^ protocolVersion = 5.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
clientID: arg1
	self socketClient clientID: arg1.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
connect
	self socketClient connect.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
debugLog: arg1
	self class debugLog: arg1.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
disconnect
	self socketClient ifNotNil: [ :temp1 |
		temp1 disconnect ].! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
openOnHostName: arg1 port: arg2 keepAlive: arg3
	self hostName: arg1.
	self keepAliveTime: arg3.
	self socketClient:
		(self socketClientClass
			openOnHostName: arg1
			port: arg2
			keepAlive: arg3
			interface: self).! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
publish: arg1 onTopic: arg2 qos: arg3
	self
		publish: arg1
		onTopic: arg2
		qos: arg3
		retain: false.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
publish: arg1 onTopic: arg2 qos: arg3 retain: arg4
	self socketClient
		publishTopic: arg2
		message: arg1
		qos: arg3
		retain: arg4.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
readWaitTime: arg1
	self socketClient readWaitTime: arg1.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
stop
	self disconnect.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
subscribeTo: arg1 qos: arg2 do: arg3
	self socketClient
		onTopic: arg1
		qos: arg2
		do: arg3.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
unsubscribeFrom: arg1
	self socketClient unsubscribeFrom: arg1.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: ''!
username: arg1 password: arg2
	self socketClient
		username: arg1
		password: arg2.! !

!MQTTClientInterface class methodsFor: 'instance creation' stamp: ''!
openOnHostName: arg1 port: arg2 keepAlive: arg3
	| temp4 temp5 |
	temp4 := self new.
	temp5 := arg3 > 0
		ifTrue: [ arg3 max: 60 ]
		ifFalse: [ 0 ].
	^ temp4
		openOnHostName: arg1
		port: arg2
		keepAlive: temp5.! !

!MQTTClientInterface class methodsFor: 'mqtt' stamp: ''!
debugLog: arg1
	self
		debugLog: arg1
		tag: ' CI '
		str2: ''.! !

!MQTTClientInterface class methodsFor: 'mqtt' stamp: ''!
debugLog: arg1 tag: arg2 str2: arg3
	| temp4 |
	temp4 := WriteStream on: (String new: 128).
	Time now
		print24: true
		showSeconds: true
		on: temp4.
	temp4
		nextPutAll: arg2;
		nextPutAll: arg1;
		space;
		nextPutAll: arg3.
	TranscriptMutex ifNil: [ TranscriptMutex := Semaphore forMutualExclusion ].
	TranscriptMutex critical: [
		Transcript
			show: temp4 contents;
			cr ].! !

!MQTTPacket methodsFor: 'mqtt' stamp: ''!
badConnectAcknowledgeError
	self debugLog: 'badConnectAcknowledgeError'.
	MQTTConnectionException signal: 'badConnectAcknowledgeError'.! !

!MQTTPacket methodsFor: 'mqtt' stamp: ''!
badFixedHeaderError
	self debugLog: 'badFixedHeaderError'.
	^ MQTTBadHeaderException signal.! !

!MQTTPacket methodsFor: 'mqtt' stamp: ''!
badQosError
	self debugLog: 'badQosError'.
	MQTTBadHeaderException signal: 'badQosError'.! !

!MQTTPacket methodsFor: 'mqtt' stamp: ''!
badTopicError
	self debugLog: 'badTopicError'.
	MQTTBadTopicException signal: 'badTopicError'.! !

!MQTTPacket methodsFor: 'mqtt' stamp: ''!
badTopicListError
	self debugLog: 'badTopicListError'.
	MQTTBadTopicException signal: 'badTopicListError'.! !

!MQTTPacket methodsFor: 'mqtt' stamp: ''!
connectionRefusedError: arg1
	self debugLog: 'connectionRefusedError:'.
	^ MQTTCONNECTException connectReturnCode: arg1.! !

!MQTTPacket methodsFor: 'mqtt' stamp: ''!
debugLog: arg1
	self class debugLog: arg1.! !

!MQTTPacket methodsFor: 'mqtt' stamp: ''!
encodedLengthError
	self debugLog: 'encodedLengthError'.
	MQTTBadHeaderException signal: 'encodedLengthError'.! !

!MQTTPacket methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	^ self subclassResponsibility.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
decode16BitIntegerFrom: arg1
	^ arg1 next << 8 + arg1 next.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
decode32BitIntegerFrom: arg1
	^ arg1 next << 24 + (arg1 next << 16) + (arg1 next << 8) + arg1 next.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
decodeBinaryDataFrom: arg1
	| temp2 |
	temp2 := self decode16BitIntegerFrom: arg1.
	^ arg1 next: temp2.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
decodeFixedHeaderFrom: arg1
	| temp2 |
	temp2 := arg1 next.
	temp2 = self fixedHeader ifFalse: [ self badFixedHeaderError ].
	self decodeLengthFrom: arg1.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
decodeFrom: arg1
	self decodeFixedHeaderFrom: arg1.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
decodeLengthFrom: arg1
	| temp2 |
	temp2 := arg1 next.
	temp2 > 0 ifTrue: [ self encodedLengthError ].! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
decodeStringFrom: arg1
	| temp2 temp3 |
	temp2 := self decode16BitIntegerFrom: arg1.
	temp3 := arg1 next: temp2.
	^ UTF8Encoder decode: temp3.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
decodeVariableByteIntegerFrom: arg1
	| temp2 temp3 temp4 |
	temp3 := 0.
	temp4 := 0.
	[
	temp2 := arg1 next.
	(temp2 isNil or: [ temp3 > 21 ]) ifTrue: [ self encodedLengthError ].
	temp4 := temp4 + ((temp2 bitAnd: 127) << temp3).
	temp3 := temp3 + 7.
	(temp2 bitAnd: 128) > 0 ] whileTrue.
	^ temp4.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
encode16BitInteger: arg1 on: arg2
	arg2
		nextPut: arg1 >> 8;
		nextPut: (arg1 bitAnd: 255).! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
encode32BitInteger: arg1 on: arg2
	arg2
		nextPut: (arg1 >> 24 bitAnd: 255);
		nextPut: (arg1 >> 16 bitAnd: 255);
		nextPut: (arg1 >> 8 bitAnd: 255);
		nextPut: (arg1 bitAnd: 255).! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
encodeBinaryData: arg1 on: arg2
	self
		encode16BitInteger: arg1 size
		on: arg2.
	arg2 nextPutAll: arg1.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
encodeOn: arg1
	arg1
		nextPut: self fixedHeader;
		nextPut: 0;
		flush.
	^ arg1.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
encodeString: arg1 on: arg2
	| temp3 |
	temp3 := UTF8Encoder encode: arg1 asString.
	self
		encode16BitInteger: temp3 size
		on: arg2.
	arg2 nextPutAll: temp3.! !

!MQTTPacket methodsFor: 'encoding' stamp: 'Simba 2/16/2026 05:40'!
encodeVariableByteInteger: anInteger on: aStream
	"Alias for encodeVariableLength:on: - needed by MQTTPacketSubscribe properties encoding"
	self encodeVariableLength: anInteger on: aStream.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
encodeVariableLength: arg1 on: arg2
	| temp3 temp4 |
	temp3 := arg1.
	[
	temp4 := temp3 \\ 128.
	temp3 := temp3 // 128.
	temp3 > 0 ifTrue: [ temp4 := temp4 bitOr: 128 ].
	arg2 nextPut: temp4.
	temp3 > 0 ] whileTrue.! !

!MQTTPacket methodsFor: 'encoding' stamp: ''!
readFrom: arg1
	self decodeFrom: arg1.! !

!MQTTPacket methodsFor: 'accessing' stamp: ''!
fixedHeader
	^ self packetType << 4 bitOr: self fixedHeaderFlags.! !

!MQTTPacket methodsFor: 'accessing' stamp: ''!
fixedHeaderFlags
	^ 0.! !

!MQTTPacket methodsFor: 'accessing' stamp: ''!
packetType
	^ nil.! !

!MQTTPacket class methodsFor: 'class initialization' stamp: ''!
initialize
	PacketTypes := OrderedCollection new
		add: MQTTPacketConnect;
		add: MQTTPacketConnAck;
		add: MQTTPacketPublish;
		add: MQTTPacketPubAck;
		add: MQTTPacketPubRec;
		add: MQTTPacketPubRel;
		add: MQTTPacketPubComp;
		add: MQTTPacketSubscribe;
		add: MQTTPacketSubAck;
		add: MQTTPacketUnsubscribe;
		add: MQTTPacketUnsubAck;
		add: MQTTPacketPingReq;
		add: MQTTPacketPingResp;
		add: MQTTPacketDisconnect;
		add: MQTTPacketAuth;
		yourself.
	MQTTProtocolLevel311 := 4.
	MQTTProtocolLevel50 := 5.! !

!MQTTPacket class methodsFor: 'accessing' stamp: ''!
protocolLevel311
	^ MQTTProtocolLevel311.! !

!MQTTPacket class methodsFor: 'accessing' stamp: ''!
protocolLevel50
	^ MQTTProtocolLevel50.! !

!MQTTPacket class methodsFor: 'mqtt' stamp: ''!
badPacketTypeError
	self debugLog: 'badPacketTypeError'.
	MQTTBadPacketTypeException signal.! !

!MQTTPacket class methodsFor: 'mqtt' stamp: ''!
debugLog: arg1
	MQTTClientInterface
		debugLog: arg1
		tag: ' PP '
		str2: ''.! !

!MQTTPacket class methodsFor: 'mqtt' stamp: ''!
emptyStreamError
	^ MQTTEmptyStreamErrorException signal: 'MQTT no bytes available to be read'.! !

!MQTTPacket class methodsFor: 'mqtt' stamp: ''!
readFrom: arg1
	| temp2 |
	temp2 := [ arg1 peek ]
		on: ConnectionTimedOut
		do: [ :argm0_3 |
			self emptyStreamError ].
	temp2 ifNil: [ ^ self emptyStreamError ].
	^ (PacketTypes
		at: temp2 >> 4
		ifAbsent: [ self badPacketTypeError ]) new readFrom: arg1.! !

!MQTTPacketAuth methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCode := 0.! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
authenticationData
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 22.! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
authenticationData: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 22
			value: arg1).! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
authenticationMethod
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 21.! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
authenticationMethod: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 21
			value: arg1).! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
packetType
	^ 15.! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
properties
	^ properties.! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
properties: arg1
	properties := arg1.! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
reasonCode
	^ reasonCode.! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
reasonCode: arg1
	reasonCode := arg1.! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
reasonString
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 31.! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: ''!
reasonString: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 31
			value: arg1).! !

!MQTTPacketAuth methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handleAuthPacket: self.! !

!MQTTPacketAuth methodsFor: 'testing' stamp: ''!
isContinueAuthentication
	^ reasonCode = 24.! !

!MQTTPacketAuth methodsFor: 'testing' stamp: ''!
isReauthenticate
	^ reasonCode = 25.! !

!MQTTPacketAuth methodsFor: 'testing' stamp: ''!
isSuccess
	^ reasonCode = 0.! !

!MQTTPacketAuth methodsFor: 'encoding' stamp: ''!
decodeBinaryDataFrom: arg1
	| temp2 |
	temp2 := (arg1 next bitShift: 8) bitOr: arg1 next.
	^ arg1 next: temp2.! !

!MQTTPacketAuth methodsFor: 'encoding' stamp: ''!
decodeFrom: arg1 remainingLength: arg2
	remainingLength := arg2.
	arg2 = 0 ifTrue: [
		reasonCode := 0.
		^ self ].
	reasonCode := arg1 next.
	arg2 = 1 ifTrue: [ ^ self ].
	self decodePropertiesFrom: arg1.! !

!MQTTPacketAuth methodsFor: 'encoding' stamp: ''!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	properties := MQTT5Properties new.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 21
			ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp4
						value: (self decodeStringFrom: arg1)) ]
			ifFalse: [
				temp4 = 22
					ifTrue: [
						properties addProperty:
							(MQTT5Property
								identifier: temp4
								value: (self decodeBinaryDataFrom: arg1)) ]
					ifFalse: [
						temp4 = 31
							ifTrue: [
								properties addProperty:
									(MQTT5Property
										identifier: temp4
										value: (self decodeStringFrom: arg1)) ]
							ifFalse: [
								temp4 = 38 ifTrue: [
									properties addProperty:
										(MQTT5Property
											identifier: temp4
											value:
												(Array
													with: (self decodeStringFrom: arg1)
													with: (self decodeStringFrom: arg1))) ]]]]].! !

!MQTTPacketAuth methodsFor: 'encoding' stamp: ''!
decodeStringFrom: arg1
	| temp2 temp3 |
	temp2 := (arg1 next bitShift: 8) bitOr: arg1 next.
	temp3 := arg1 next: temp2.
	^ temp3 asString.! !

!MQTTPacketAuth methodsFor: 'encoding' stamp: ''!
decodeVariableByteIntegerFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := 1.
	temp3 := 0.
	[
	temp4 := arg1 next.
	temp3 := temp3 + ((temp4 bitAnd: 127) * temp2).
	temp2 := temp2 * 128.
	(temp4 bitAnd: 128) ~= 0 ] whileTrue.
	^ temp3.! !

!MQTTPacketAuth methodsFor: 'encoding' stamp: ''!
encodeOn: arg1
	| temp2 |
	temp2 := MQTTWriteStream on: (ByteArray new: 256).
	temp2 nextPut: (reasonCode ifNil: [ 0 ]).
	(properties notNil and: [ properties isEmpty not ])
		ifTrue: [ properties encodeOn: temp2 ]
		ifFalse: [ temp2 nextPut: 0 ].
	arg1 nextPut: (self packetType bitShift: 4).
	self
		encodeVariableLength: temp2 position
		on: arg1.
	arg1 nextPutAll: temp2 contents.! !

!MQTTPacketAuth methodsFor: 'encoding' stamp: ''!
encodeVariableLength: arg1 on: arg2
	| temp3 temp4 |
	temp3 := arg1.
	[
	temp4 := temp3 \\ 128.
	temp3 := temp3 // 128.
	temp3 > 0 ifTrue: [ temp4 := temp4 bitOr: 128 ].
	arg2 nextPut: temp4.
	temp3 > 0 ] whileTrue.! !

!MQTTPacketDisconnect methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCode := 0.! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
packetType
	^ 14.! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
properties
	^ properties.! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
properties: arg1
	properties := arg1.! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
reasonCode
	^ reasonCode.! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
reasonCode: arg1
	reasonCode := arg1.! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
reasonString
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 31.! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
reasonString: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 31
			value: arg1).! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
serverReference
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 28.! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
sessionExpiryInterval
	properties ifNil: [ ^ nil ].
	^ properties propertyAt: 17.! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: ''!
sessionExpiryInterval: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: 17
			value: arg1).! !

!MQTTPacketDisconnect methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handleDisconnectPacket: self.! !

!MQTTPacketDisconnect methodsFor: 'encoding' stamp: ''!
decode32BitIntegerFrom: arg1
	^ ((arg1 next bitShift: 24) bitOr: (arg1 next bitShift: 16)) bitOr: ((arg1 next bitShift: 8) bitOr: arg1 next).! !

!MQTTPacketDisconnect methodsFor: 'encoding' stamp: ''!
decodeFrom: arg1 remainingLength: arg2
	arg2 = 0 ifTrue: [
		reasonCode := 0.
		^ self ].
	reasonCode := arg1 next.
	arg2 = 1 ifTrue: [ ^ self ].
	self decodePropertiesFrom: arg1.! !

!MQTTPacketDisconnect methodsFor: 'encoding' stamp: ''!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	properties := MQTT5Properties new.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 17
			ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp4
						value: (self decode32BitIntegerFrom: arg1)) ]
			ifFalse: [
				temp4 = 31
					ifTrue: [
						properties addProperty:
							(MQTT5Property
								identifier: temp4
								value: (self decodeStringFrom: arg1)) ]
					ifFalse: [
						temp4 = 28
							ifTrue: [
								properties addProperty:
									(MQTT5Property
										identifier: temp4
										value: (self decodeStringFrom: arg1)) ]
							ifFalse: [
								temp4 = 38 ifTrue: [
									properties addProperty:
										(MQTT5Property
											identifier: temp4
											value:
												(Array
													with: (self decodeStringFrom: arg1)
													with: (self decodeStringFrom: arg1))) ]]]]].! !

!MQTTPacketDisconnect methodsFor: 'encoding' stamp: ''!
decodeStringFrom: arg1
	| temp2 temp3 |
	temp2 := (arg1 next bitShift: 8) bitOr: arg1 next.
	temp3 := arg1 next: temp2.
	^ temp3 asString.! !

!MQTTPacketDisconnect methodsFor: 'encoding' stamp: ''!
decodeVariableByteIntegerFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := 1.
	temp3 := 0.
	[
	temp4 := arg1 next.
	temp3 := temp3 + ((temp4 bitAnd: 127) * temp2).
	temp2 := temp2 * 128.
	(temp4 bitAnd: 128) ~= 0 ] whileTrue.
	^ temp3.! !

!MQTTPacketDisconnect methodsFor: 'encoding' stamp: ''!
encodeOn: arg1
	| temp2 |
	temp2 := MQTTWriteStream on: (ByteArray new: 64).
	((reasonCode isNil or: [ reasonCode = 0 ]) and: [
		properties isNil or: [ properties isEmpty ]]) ifTrue: [
		arg1 nextPut: (self packetType bitShift: 4).
		arg1 nextPut: 0.
		^ self ].
	temp2 nextPut: (reasonCode ifNil: [ 0 ]).
	(properties notNil and: [ properties isEmpty not ])
		ifTrue: [ properties encodeOn: temp2 ]
		ifFalse: [ temp2 nextPut: 0 ].
	arg1 nextPut: (self packetType bitShift: 4).
	self
		encodeVariableLength: temp2 position
		on: arg1.
	arg1 nextPutAll: temp2 contents.! !

!MQTTPacketDisconnect methodsFor: 'encoding' stamp: ''!
encodeVariableLength: arg1 on: arg2
	| temp3 temp4 |
	temp3 := arg1.
	[
	temp4 := temp3 \\ 128.
	temp3 := temp3 // 128.
	temp3 > 0 ifTrue: [ temp4 := temp4 bitOr: 128 ].
	arg2 nextPut: temp4.
	temp3 > 0 ] whileTrue.! !

!MQTTPacketPingReq methodsFor: 'accessing' stamp: ''!
packetType
	^ 12.! !

!MQTTPacketPingReq methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handlePingReqPacket: self.! !

!MQTTPacketPingReq methodsFor: 'mqtt' stamp: ''!
pendingJob
	^ MQTTPendingPingJob for: self.! !

!MQTTPacketPingReq methodsFor: 'encoding' stamp: ''!
encodeOn: arg1
	arg1 nextPut: (self packetType bitShift: 4).
	arg1 nextPut: 0.! !

!MQTTPacketPingResp methodsFor: 'accessing' stamp: ''!
packetType
	^ 13.! !

!MQTTPacketPingResp methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handlePingRespPacket: self.! !

!MQTTPacketPingResp methodsFor: 'encoding' stamp: ''!
encodeOn: arg1
	arg1 nextPut: (self packetType bitShift: 4).
	arg1 nextPut: 0.! !

!MQTTPacketVariableHeaded methodsFor: 'initialization' stamp: ''!
initialize
	properties := MQTT5Properties new.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
decodeByteArrayFrom: arg1
	| temp2 temp3 |
	temp2 := self decode16BitIntegerFrom: arg1.
	temp3 := arg1 next: temp2.
	^ temp3.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
decodeFrom: arg1
	super decodeFrom: arg1.
	self decodeVariableHeaderFrom: arg1.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
decodeLengthFrom: arg1
	| temp2 temp3 temp4 |
	temp3 := 0.
	temp4 := 0.
	[
	temp2 := arg1 next.
	(temp2 isNil or: [ temp3 > 21 ]) ifTrue: [ self encodedLengthError ].
	temp4 := temp4 + ((temp2 bitAnd: 127) << temp3).
	temp3 := temp3 + 7.
	(temp2 bitAnd: 128) > 0 ] whileTrue.
	remainingLength := temp4.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
decodePropertiesFrom: arg1
	properties := MQTT5Properties new.
	properties decodeFrom: arg1.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
decodeStringFrom: arg1
	| temp2 temp3 |
	temp2 := self decode16BitIntegerFrom: arg1.
	temp3 := arg1 next: temp2.
	^ UTF8Encoder decode: temp3.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
encodeFixedHeaderOn: arg1
	arg1 nextPut: self fixedHeader.
	self encodeLengthOn: arg1.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
encodeLengthOn: arg1
	| temp2 temp3 |
	temp2 := remainingLength.
	[
	temp3 := temp2 \\ 128.
	temp2 := temp2 // 128.
	temp2 > 0 ifTrue: [ temp3 := temp3 bitOr: 128 ].
	arg1 nextPut: temp3.
	temp2 > 0 ] whileTrue.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
encodeOn: arg1
	| temp2 |
	temp2 := MQTTWriteStream on: (ByteArray new: self guessEncodedLength).
	self encodeVariableHeaderOn: temp2.
	self encodePayloadOn: temp2.
	remainingLength := temp2 position.
	self encodeFixedHeaderOn: arg1.
	arg1
		next: temp2 position
		putAll: temp2 contents
		startingAt: 1.
	arg1 flush.
	^ arg1.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
encodePayloadOn: arg1
	^ self.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
encodePropertiesOn: arg1
	properties ifNil: [ properties := MQTT5Properties new ].
	properties encodeOn: arg1.! !

!MQTTPacketVariableHeaded methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.! !

!MQTTPacketVariableHeaded methodsFor: 'accessing' stamp: ''!
guessEncodedLength
	^ 100.! !

!MQTTPacketVariableHeaded methodsFor: 'accessing' stamp: ''!
packetID
	^ msgID.! !

!MQTTPacketVariableHeaded methodsFor: 'accessing' stamp: ''!
packetID: arg1
	msgID := arg1.! !

!MQTTPacketVariableHeaded methodsFor: 'accessing' stamp: ''!
properties
	^ properties ifNil: [ properties := MQTT5Properties new ].! !

!MQTTPacketVariableHeaded methodsFor: 'accessing' stamp: ''!
properties: arg1
	properties := arg1.! !

!MQTTPacketVariableHeaded methodsFor: 'accessing' stamp: ''!
remainingLength
	^ remainingLength.! !

!MQTTPacketVariableHeaded methodsFor: 'printing' stamp: ''!
printOn: arg1
	super printOn: arg1.
	msgID ifNotNil: [
		arg1 nextPutAll: ' msgID: '.
		msgID printOn: arg1 ].! !

!MQTTPacketConnAck methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCode := 0.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
byte1
	^ byte1.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
byte2
	^ byte2.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
byte2: arg1
	byte2 := arg1.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
packetType
	^ 2.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
reasonCode
	^ reasonCode.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
reasonCode: arg1
	reasonCode := arg1.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
returnCode
	^ byte2.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
returnCode: arg1
	byte2 := arg1.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
sessionPresent
	^ sessionPresent.! !

!MQTTPacketConnAck methodsFor: 'accessing' stamp: ''!
sessionPresent: arg1
	sessionPresent := arg1.! !

!MQTTPacketConnAck methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	byte1 := arg1 next.
	(byte1 anyMask: 254) ifTrue: [ ^ self badConnectAcknowledgeError ].
	sessionPresent := byte1 allMask: 1.
	byte2 := arg1 next.
	reasonCode := byte2.
	remainingLength > 2 ifTrue: [ self decodePropertiesFrom: arg1 ].! !

!MQTTPacketConnAck methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	byte1 := sessionPresent asBit.
	arg1
		nextPut: sessionPresent asBit;
		nextPut: (reasonCode ifNil: [ byte2 ifNil: [ 0 ]]).! !

!MQTTPacketConnAck methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handleConnAckPacket: self.! !

!MQTTPacketPubAck methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCode := 0.! !

!MQTTPacketPubAck methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handlePubAckPacket: self.! !

!MQTTPacketPubAck methodsFor: 'accessing' stamp: ''!
packetType
	^ 4.! !

!MQTTPacketPubAck methodsFor: 'accessing' stamp: ''!
reasonCode
	^ reasonCode.! !

!MQTTPacketPubAck methodsFor: 'accessing' stamp: ''!
reasonCode: arg1
	reasonCode := arg1.! !

!MQTTPacketPubAck methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	remainingLength >= 3 ifTrue: [ reasonCode := arg1 next ].
	remainingLength > 3 ifTrue: [ self decodePropertiesFrom: arg1 ].! !

!MQTTPacketPubAck methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	(reasonCode notNil and: [ reasonCode ~= 0 ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].! !

!MQTTPacketPubComp methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCode := 0.! !

!MQTTPacketPubComp methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handlePubCompPacket: self.! !

!MQTTPacketPubComp methodsFor: 'accessing' stamp: ''!
packetType
	^ 7.! !

!MQTTPacketPubComp methodsFor: 'accessing' stamp: ''!
reasonCode
	^ reasonCode.! !

!MQTTPacketPubComp methodsFor: 'accessing' stamp: ''!
reasonCode: arg1
	reasonCode := arg1.! !

!MQTTPacketPubComp methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	remainingLength >= 3 ifTrue: [ reasonCode := arg1 next ].
	remainingLength > 3 ifTrue: [ self decodePropertiesFrom: arg1 ].! !

!MQTTPacketPubComp methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	(reasonCode notNil and: [ reasonCode ~= 0 ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].! !

!MQTTPacketPubRec methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCode := 0.! !

!MQTTPacketPubRec methodsFor: 'mqtt' stamp: ''!
acknowledgement
	^ MQTTPacketPubRel new packetID: msgID.! !

!MQTTPacketPubRec methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handlePubRecPacket: self.! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: ''!
originalPacket
	^ originalPacket.! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: ''!
originalPacket: arg1
	originalPacket := arg1.! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: ''!
packetType
	^ 5.! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: ''!
reasonCode
	^ reasonCode.! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: ''!
reasonCode: arg1
	reasonCode := arg1.! !

!MQTTPacketPubRec methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	remainingLength >= 3 ifTrue: [ reasonCode := arg1 next ].
	remainingLength > 3 ifTrue: [ self decodePropertiesFrom: arg1 ].! !

!MQTTPacketPubRec methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	(reasonCode notNil and: [ reasonCode ~= 0 ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].! !

!MQTTPacketPubRel methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCode := 0.! !

!MQTTPacketPubRel methodsFor: 'mqtt' stamp: ''!
acknowledgement
	^ MQTTPacketPubComp new packetID: msgID.! !

!MQTTPacketPubRel methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handlePubRelPacket: self.! !

!MQTTPacketPubRel methodsFor: 'mqtt' stamp: ''!
pendingJob
	^ MQTTPendingPubCompJob for: self.! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: ''!
fixedHeaderFlags
	^ 2.! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: ''!
packetType
	^ 6.! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: ''!
reasonCode
	^ reasonCode.! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: ''!
reasonCode: arg1
	reasonCode := arg1.! !

!MQTTPacketPubRel methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	remainingLength >= 3 ifTrue: [ reasonCode := arg1 next ].
	remainingLength > 3 ifTrue: [ self decodePropertiesFrom: arg1 ].! !

!MQTTPacketPubRel methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	(reasonCode notNil and: [ reasonCode ~= 0 ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].! !

!MQTTPacketUnsubAck methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCodes := OrderedCollection new.! !

!MQTTPacketUnsubAck methodsFor: 'accessing' stamp: ''!
packetType
	^ 11.! !

!MQTTPacketUnsubAck methodsFor: 'accessing' stamp: ''!
reasonCodes
	^ reasonCodes.! !

!MQTTPacketUnsubAck methodsFor: 'accessing' stamp: ''!
reasonCodes: arg1
	reasonCodes := arg1.! !

!MQTTPacketUnsubAck methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handleUnsubAckPacket: self.! !

!MQTTPacketUnsubAck methodsFor: 'testing' stamp: ''!
hasFailures
	^ reasonCodes anySatisfy: [ :argm0_1 |
		argm0_1 >= 128 ].! !

!MQTTPacketUnsubAck methodsFor: 'testing' stamp: ''!
isSuccess
	^ reasonCodes allSatisfy: [ :argm0_1 |
		argm0_1 = 0 ].! !

!MQTTPacketUnsubAck methodsFor: 'encoding' stamp: ''!
decodePayloadFrom: arg1 length: arg2
	reasonCodes := OrderedCollection new.
	arg2 timesRepeat: [ reasonCodes add: arg1 next ].! !

!MQTTPacketUnsubAck methodsFor: 'encoding' stamp: ''!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	properties := MQTT5Properties new.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 31
			ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp4
						value: (self decodeStringFrom: arg1)) ]
			ifFalse: [
				temp4 = 38 ifTrue: [
					properties addProperty:
						(MQTT5Property
							identifier: temp4
							value:
								(Array
									with: (self decodeStringFrom: arg1)
									with: (self decodeStringFrom: arg1))) ]]].! !

!MQTTPacketUnsubAck methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	self decodePropertiesFrom: arg1.! !

!MQTTPacketUnsubAck methodsFor: 'encoding' stamp: ''!
encodePayloadOn: arg1
	reasonCodes do: [ :argm0_2 |
		arg1 nextPut: argm0_2 ].! !

!MQTTPacketUnsubAck methodsFor: 'encoding' stamp: ''!
encodePropertiesOn: arg1
	(properties isNil or: [ properties isEmpty ]) ifTrue: [
		arg1 nextPut: 0.
		^ self ].
	properties encodeOn: arg1.! !

!MQTTPacketUnsubAck methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	self encodePropertiesOn: arg1.! !

!MQTTPacketVariableHeadedWithPayload methodsFor: 'encoding' stamp: ''!
decodeFrom: arg1
	super decodeFrom: arg1.
	self decodePayloadFrom: arg1.! !

!MQTTPacketVariableHeadedWithPayload methodsFor: 'encoding' stamp: ''!
decodePayloadFrom: arg1
	^ self.! !

!MQTTPacketVariableHeadedWithPayload methodsFor: 'encoding' stamp: ''!
encodePayloadOn: arg1
	^ self.! !

!MQTTPacketVariableHeadedWithPayload methodsFor: 'accessing' stamp: ''!
messageID
	^ msgID.! !

!MQTTPacketVariableHeadedWithPayload methodsFor: 'accessing' stamp: ''!
messageID: arg1
	msgID := arg1.! !

!MQTTPacketConnect methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	willRetainFlag := false.
	cleanSessionFlag := true.
	willQos := 0.
	keepAliveTime := 60.
	protocolVersion := MQTTProtocolLevel50.
	willProperties := MQTT5Properties new.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
authenticationData
	^ authenticationData.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
authenticationData: arg1
	authenticationData := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
authenticationMethod
	^ authenticationMethod.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
authenticationMethod: arg1
	authenticationMethod := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
cleanSessionFlag: arg1
	cleanSessionFlag := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
clientID: arg1
	clientID := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
clientIDString
	^ clientID ifNil: [ 'SqueakMQTT5' ].! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
keepAliveTime
	^ keepAliveTime.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
keepAliveTime: arg1
	keepAliveTime := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
maximumPacketSize
	^ maximumPacketSize.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
maximumPacketSize: arg1
	maximumPacketSize := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
packetType
	^ 1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
password
	^ password.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
password: arg1
	password := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
protocolLevel: arg1
	protocolVersion := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
protocolVersion
	^ protocolVersion.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
protocolVersion: arg1
	protocolVersion := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
receiveMaximum
	^ receiveMaximum.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
receiveMaximum: arg1
	receiveMaximum := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
requestProblemInfo
	^ requestProblemInfo.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
requestProblemInfo: arg1
	requestProblemInfo := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
requestResponseInfo
	^ requestResponseInfo.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
requestResponseInfo: arg1
	requestResponseInfo := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
sessionExpiryInterval
	^ sessionExpiryInterval.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
sessionExpiryInterval: arg1
	sessionExpiryInterval := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
topicAliasMaximum
	^ topicAliasMaximum.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
topicAliasMaximum: arg1
	topicAliasMaximum := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
user
	^ user.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
user: arg1
	user := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
user: arg1 password: arg2
	user := arg1.
	password := arg2.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
willProperties
	^ willProperties ifNil: [ willProperties := MQTT5Properties new ].! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
willProperties: arg1
	willProperties := arg1.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: ''!
willTopic: arg1 message: arg2 retain: arg3 qos: arg4
	willTopic := arg1.
	willMessage := arg2.
	willRetainFlag := arg3.
	willQos := arg4
		min: 2
		max: 0.! !

!MQTTPacketConnect methodsFor: 'private' stamp: ''!
connectFlags
	| temp1 |
	temp1 := willQos << 3.
	temp1 := temp1
		bitAt: 2
		put: cleanSessionFlag asBit.
	temp1 := temp1
		bitAt: 3
		put: willTopic notNil asBit.
	temp1 := temp1
		bitAt: 6
		put: willRetainFlag asBit.
	temp1 := temp1
		bitAt: 7
		put: password notNil asBit.
	temp1 := temp1
		bitAt: 8
		put: user notNil asBit.
	^ temp1.! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: ''!
decodePayloadFrom: arg1
	| temp2 temp3 temp4 temp5 temp6 |
	temp5 := self decodeStringFrom: arg1.
	temp5 = 'MQTT' ifFalse: [
		self debugLog: 'Protocol found: ' , temp5.
		(self connectionRefusedError: 1) signal ].
	temp6 := arg1 next.
	protocolVersion := temp6.
	(temp6 = 4 or: [ temp6 = 5 ]) ifFalse: [ self debugLog: 'version found: ' , temp6 ].
	temp2 := arg1 next.
	(temp2 allMask: 1) ifTrue: [ (self connectionRefusedError: 1) signal ].
	cleanSessionFlag := temp2 allMask: 2.
	willFlag := temp2 allMask: 4.
	willRetainFlag := temp2 allMask: 1 << 5.
	willQos := temp2 >> 3 bitAnd: 3.
	temp3 := temp2 allMask: 1 << 6.
	temp4 := temp2 allMask: 1 << 7.
	keepAliveTime := self decode16BitIntegerFrom: arg1.
	temp6 = 5 ifTrue: [ self decodePropertiesFrom: arg1 ].
	clientID := self decodeStringFrom: arg1.
	clientID size = 0 ifTrue: [ (self connectionRefusedError: 2) signal ].
	willFlag ifTrue: [
		temp6 = 5 ifTrue: [
			willProperties := MQTT5Properties new.
			willProperties decodeFrom: arg1 ].
		willTopic := self decodeStringFrom: arg1.
		willMessage := self decodeByteArrayFrom: arg1 ].
	temp4 ifTrue: [ user := self decodeStringFrom: arg1 ].
	temp3 ifTrue: [ password := self decodeByteArrayFrom: arg1 ].
	returnCode := 0.! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	^ self.! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: ''!
encodeConnectPropertiesOn: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	sessionExpiryInterval ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants sessionExpiryInterval
			value: sessionExpiryInterval ].
	receiveMaximum ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants receiveMaximum
			value: receiveMaximum ].
	maximumPacketSize ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants maximumPacketSize
			value: maximumPacketSize ].
	topicAliasMaximum ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants topicAliasMaximum
			value: topicAliasMaximum ].
	requestResponseInfo ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants requestResponseInformation
			value: requestResponseInfo asBit ].
	requestProblemInfo ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants requestProblemInformation
			value: requestProblemInfo asBit ].
	authenticationMethod ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants authenticationMethod
			value: authenticationMethod ].
	authenticationData ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants authenticationData
			value: authenticationData ].
	properties do: [ :argm9_3 |
		argm9_3 identifier = MQTT5Constants userProperty ifTrue: [ temp2 add: argm9_3 ]].
	temp2 encodeOn: arg1.! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: ''!
encodePasswordOn: arg1
	password ifNotNil: [
		self
			encodeString: password
			on: arg1 ].! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: ''!
encodePayloadOn: arg1
	self
		encodeString: self clientIDString
		on: arg1.
	self encodeWillDataOn: arg1.
	self encodeUsernameOn: arg1.
	self encodePasswordOn: arg1.! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: ''!
encodeUsernameOn: arg1
	user ifNotNil: [
		self
			encodeString: user
			on: arg1 ].! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encodeString: 'MQTT'
		on: arg1.
	arg1
		nextPut: protocolVersion;
		nextPut: self connectFlags.
	self
		encode16BitInteger: keepAliveTime
		on: arg1.
	protocolVersion = 5 ifTrue: [ self encodeConnectPropertiesOn: arg1 ].! !

!MQTTPacketConnect methodsFor: 'encoding' stamp: ''!
encodeWillDataOn: arg1
	willTopic ifNotNil: [
		protocolVersion = 5 ifTrue: [ willProperties encodeOn: arg1 ].
		self
			encodeString: willTopic
			on: arg1.
		willMessage
			ifNil: [
				self
					encodeString: ''
					on: arg1 ]
			ifNotNil: [
				self
					encodeString: willMessage
					on: arg1 ]].! !

!MQTTPacketConnect methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	^ arg1 handleConnectPacket: self.! !

!MQTTPacketPublish methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	qos := 0.
	duplicate := retain := false.
	subscriptionIdentifiers := OrderedCollection new.! !

!MQTTPacketPublish methodsFor: 'mqtt' stamp: ''!
acknowledgement
	| temp1 |
	qos = 0 ifTrue: [ ^ self badQosError ].
	qos = 1 ifTrue: [ ^ MQTTPacketPubAck new packetID: msgID ].
	temp1 := MQTTPacketPubRec new.
	temp1 packetID: msgID.
	temp1 originalPacket: self.
	^ temp1.! !

!MQTTPacketPublish methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1
		handlePublishPacket: self
		qos: qos.! !

!MQTTPacketPublish methodsFor: 'mqtt' stamp: ''!
matchesSubscription: arg1 ifTrue: arg2
	(arg1 topicMatches: topic) ifTrue: [
		arg2
			value: topic
			value: message
			value: qos ].! !

!MQTTPacketPublish methodsFor: 'mqtt' stamp: ''!
pendingJob
	qos = 0 ifTrue: [ ^ self badQosError ].
	qos = 1 ifTrue: [ ^ MQTTPendingPubAckJob for: self ].
	^ MQTTPendingPubRecJob for: self.! !

!MQTTPacketPublish methodsFor: 'mqtt' stamp: ''!
prepareForResend
	duplicate := true.! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: ''!
decodeFixedHeaderFrom: arg1
	| temp2 |
	(temp2 := arg1 next) >> 4 = self packetType ifFalse: [ self badFixedHeaderError ].
	duplicate := temp2 allMask: 8.
	retain := temp2 allMask: 1.
	qos := temp2 >> 1 bitAnd: 3.
	self decodeLengthFrom: arg1.! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: ''!
decodeFrom: arg1
	| temp2 temp3 temp4 temp5 temp6 |
	self decodeFixedHeaderFrom: arg1.
	temp2 := self decodeVariableHeaderFrom: arg1.
	remainingLength - temp2 > 0
		ifTrue: [
			temp4 := self decodeVariableByteIntegerFrom: arg1.
			temp4 > 0 ifTrue: [
				temp5 := arg1 next: temp4.
				temp6 := ReadStream on: temp5.
				properties := MQTT5Properties new.
				[ temp6 atEnd ] whileFalse: [ properties decodePropertyFrom: temp6 ].
				(properties propertyAt: MQTT5Constants payloadFormatIndicator) ifNotNil: [ :temp7 |
					payloadFormatIndicator := temp7 ].
				(properties propertyAt: MQTT5Constants messageExpiryInterval) ifNotNil: [ :temp8 |
					messageExpiryInterval := temp8 ].
				(properties propertyAt: MQTT5Constants topicAlias) ifNotNil: [ :temp9 |
					topicAlias := temp9 ].
				(properties propertyAt: MQTT5Constants responseTopic) ifNotNil: [ :temp10 |
					responseTopic := temp10 ].
				(properties propertyAt: MQTT5Constants correlationData) ifNotNil: [ :temp11 |
					correlationData := temp11 ].
				(properties propertyAt: MQTT5Constants contentType) ifNotNil: [ :temp12 |
					contentType := temp12 ].
				subscriptionIdentifiers := properties propertiesAt: MQTT5Constants subscriptionIdentifier ].
			message := arg1 next: remainingLength - temp2 - (self variableByteIntegerSizeFor: temp4) - temp4 ]
		ifFalse: [ message := #[]].! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: ''!
decodePublishPropertiesFrom: arg1
	self decodePropertiesFrom: arg1.
	(properties propertyAt: MQTT5Constants payloadFormatIndicator) ifNotNil: [ :temp2 |
		payloadFormatIndicator := temp2 ].
	(properties propertyAt: MQTT5Constants messageExpiryInterval) ifNotNil: [ :temp3 |
		messageExpiryInterval := temp3 ].
	(properties propertyAt: MQTT5Constants topicAlias) ifNotNil: [ :temp4 |
		topicAlias := temp4 ].
	(properties propertyAt: MQTT5Constants responseTopic) ifNotNil: [ :temp5 |
		responseTopic := temp5 ].
	(properties propertyAt: MQTT5Constants correlationData) ifNotNil: [ :temp6 |
		correlationData := temp6 ].
	(properties propertyAt: MQTT5Constants contentType) ifNotNil: [ :temp7 |
		contentType := temp7 ].
	subscriptionIdentifiers := properties propertiesAt: MQTT5Constants subscriptionIdentifier.! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: ''!
decodeTopicStringFrom: arg1
	| temp2 temp3 |
	temp2 := self decode16BitIntegerFrom: arg1.
	temp3 := arg1 next: temp2.
	topic := [ UTF8Encoder decode: temp3 ]
		on: Error
		do: [ :argm2_4 |
			'Failed Topic Decode' ].
	^ temp2 + 2.! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	| temp2 |
	temp2 := self decodeTopicStringFrom: arg1.
	qos > 0 ifTrue: [
		msgID := self decode16BitIntegerFrom: arg1.
		temp2 := temp2 + 2 ].
	^ temp2.! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: ''!
encodePayloadOn: arg1
	arg1 nextPutAll: message.! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: ''!
encodePublishPropertiesOn: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	payloadFormatIndicator ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants payloadFormatIndicator
			value: payloadFormatIndicator ].
	messageExpiryInterval ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants messageExpiryInterval
			value: messageExpiryInterval ].
	topicAlias ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants topicAlias
			value: topicAlias ].
	responseTopic ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants responseTopic
			value: responseTopic ].
	correlationData ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants correlationData
			value: correlationData ].
	contentType ifNotNil: [
		temp2
			addPropertyIdentifier: MQTT5Constants contentType
			value: contentType ].
	subscriptionIdentifiers do: [ :argm7_3 |
		temp2
			addPropertyIdentifier: MQTT5Constants subscriptionIdentifier
			value: argm7_3 ].
	properties do: [ :argm8_3 |
		argm8_3 identifier = MQTT5Constants userProperty ifTrue: [ temp2 add: argm8_3 ]].
	temp2 encodeOn: arg1.! !

!MQTTPacketPublish methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	(topicAlias notNil and: [
		topic isNil or: [ topic isEmpty ]])
		ifTrue: [
			self
				encode16BitInteger: 0
				on: arg1 ]
		ifFalse: [
			topic ifNil: [ ^ self badTopicError ].
			self
				encodeString: topic
				on: arg1 ].
	qos > 0 ifTrue: [
		self
			encode16BitInteger: msgID
			on: arg1 ].
	self encodePublishPropertiesOn: arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
addSubscriptionIdentifier: arg1
	subscriptionIdentifiers add: arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
contentType
	^ contentType.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
contentType: arg1
	contentType := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
correlationData
	^ correlationData.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
correlationData: arg1
	correlationData := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
duplicateFlag: arg1
	duplicate := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
fixedHeaderFlags
	^ ((duplicate
		ifTrue: [ 8 ]
		ifFalse: [ 0 ]) bitOr: qos << 1) bitOr:
		(retain
			ifTrue: [ 1 ]
			ifFalse: [ 0 ]).! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
guessEncodedLength
	^ self message size + 100.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
message
	^ message.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
message: arg1
	message := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
messageExpiryInterval
	^ messageExpiryInterval.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
messageExpiryInterval: arg1
	messageExpiryInterval := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
messageString
	^ UTF8Encoder decode: message.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
packetType
	^ 3.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
payloadFormatIndicator
	^ payloadFormatIndicator.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
payloadFormatIndicator: arg1
	payloadFormatIndicator := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
qos
	^ qos.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
qos: arg1
	qos := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
responseTopic
	^ responseTopic.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
responseTopic: arg1
	responseTopic := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
retain: arg1
	retain := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
retainFlag: arg1
	retain := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
subscriptionIdentifiers
	^ subscriptionIdentifiers.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
topic
	^ topic.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
topic: arg1
	((UTF8Encoder encode: arg1 asString) size > 65535 or: [ arg1 includesAnyOf: #($# $+) ]) ifTrue: [ ^ self badTopicError ].
	topic := arg1.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
topic: arg1 message: arg2
	((UTF8Encoder encode: arg1 asString) size > 65535 or: [ arg1 includesAnyOf: #($# $+) ]) ifTrue: [ ^ self badTopicError ].
	topic := arg1.
	message := arg2.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
topicAlias
	^ topicAlias.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: ''!
topicAlias: arg1
	topicAlias := arg1.! !

!MQTTPacketPublish methodsFor: 'as yet unclassified' stamp: ''!
variableByteIntegerSizeFor: arg1
	arg1 < 128 ifTrue: [ ^ 1 ].
	arg1 < 16384 ifTrue: [ ^ 2 ].
	arg1 < 2097152 ifTrue: [ ^ 3 ].
	^ 4.! !

!MQTTPacketSubAck methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	reasonCodes := OrderedCollection new.! !

!MQTTPacketSubAck methodsFor: 'accessing' stamp: ''!
packetType
	^ 9.! !

!MQTTPacketSubAck methodsFor: 'accessing' stamp: ''!
reasonCodes
	^ reasonCodes.! !

!MQTTPacketSubAck methodsFor: 'accessing' stamp: ''!
reasonCodes: arg1
	reasonCodes := arg1.! !

!MQTTPacketSubAck methodsFor: 'accessing' stamp: ''!
returnCodes
	^ reasonCodes.! !

!MQTTPacketSubAck methodsFor: 'accessing' stamp: ''!
returnCodes: arg1
	reasonCodes := arg1.! !

!MQTTPacketSubAck methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handleSubAckPacket: self.! !

!MQTTPacketSubAck methodsFor: 'testing' stamp: ''!
hasFailures
	^ reasonCodes anySatisfy: [ :argm0_1 |
		argm0_1 >= 128 ].! !

!MQTTPacketSubAck methodsFor: 'testing' stamp: ''!
isSuccess
	^ reasonCodes allSatisfy: [ :argm0_1 |
		argm0_1 <= 2 ].! !

!MQTTPacketSubAck methodsFor: 'encoding' stamp: ''!
decodePayloadFrom: arg1 length: arg2
	reasonCodes := OrderedCollection new.
	arg2 timesRepeat: [ reasonCodes add: arg1 next ].! !

!MQTTPacketSubAck methodsFor: 'encoding' stamp: ''!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 temp5 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 next: temp2.
	temp4 := ReadStream on: temp3.
	properties := MQTT5Properties new.
	[ temp4 atEnd ] whileFalse: [
		temp5 := temp4 next.
		temp5 = 31 ifTrue: [
			properties addProperty:
				(MQTT5Property
					identifier: temp5
					value: (self decodeStringFromReadStream: temp4)) ].
		temp5 = 38 ifTrue: [
			properties addProperty:
				(MQTT5Property
					identifier: temp5
					value:
						(Array
							with: (self decodeStringFromReadStream: temp4)
							with: (self decodeStringFromReadStream: temp4))) ]].! !

!MQTTPacketSubAck methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	| temp2 temp3 temp4 temp5 |
	msgID := self decode16BitIntegerFrom: arg1.
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 > 0 ifTrue: [
		temp3 := arg1 next: temp2.
		temp4 := ReadStream on: temp3.
		properties := MQTT5Properties new.
		[ temp4 atEnd ] whileFalse: [
			temp5 := temp4 next.
			temp5 = 31 ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp5
						value: (self decodeStringFromReadStream: temp4)) ].
			temp5 = 38 ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp5
						value:
							(Array
								with: (self decodeStringFromReadStream: temp4)
								with: (self decodeStringFromReadStream: temp4))) ]]].! !

!MQTTPacketSubAck methodsFor: 'encoding' stamp: ''!
encodePayloadOn: arg1
	reasonCodes do: [ :argm0_2 |
		arg1 nextPut: argm0_2 ].! !

!MQTTPacketSubAck methodsFor: 'encoding' stamp: ''!
encodePropertiesOn: arg1
	(properties isNil or: [ properties isEmpty ]) ifTrue: [
		arg1 nextPut: 0.
		^ self ].
	properties encodeOn: arg1.! !

!MQTTPacketSubAck methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	self encodePropertiesOn: arg1.! !

!MQTTPacketSubAck methodsFor: 'as yet unclassified' stamp: ''!
decodeFrom: arg1
	| temp2 temp3 temp4 temp5 temp6 |
	super decodeFixedHeaderFrom: arg1.
	msgID := self decode16BitIntegerFrom: arg1.
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 > 0 ifTrue: [
		temp4 := arg1 next: temp2.
		temp5 := ReadStream on: temp4.
		properties := MQTT5Properties new.
		[ temp5 atEnd ] whileFalse: [
			temp6 := temp5 next.
			temp6 = 31 ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp6
						value: (self decodeStringFromReadStream: temp5)) ].
			temp6 = 38 ifTrue: [
				properties addProperty:
					(MQTT5Property
						identifier: temp6
						value:
							(Array
								with: (self decodeStringFromReadStream: temp5)
								with: (self decodeStringFromReadStream: temp5))) ]]].
	temp3 := remainingLength - 2 - (self variableByteIntegerSizeFor: temp2) - temp2.
	self
		decodePayloadFrom: arg1
		length: temp3.! !

!MQTTPacketSubAck methodsFor: 'as yet unclassified' stamp: ''!
decodeStringFromReadStream: arg1
	| temp2 temp3 |
	temp2 := (arg1 next bitShift: 8) + arg1 next.
	temp3 := arg1 next: temp2.
	^ UTF8Encoder decode: temp3.! !

!MQTTPacketSubAck methodsFor: 'as yet unclassified' stamp: ''!
variableByteIntegerSizeFor: arg1
	arg1 < 128 ifTrue: [ ^ 1 ].
	arg1 < 16384 ifTrue: [ ^ 2 ].
	arg1 < 2097152 ifTrue: [ ^ 3 ].
	^ 4.! !

!MQTTPacketSubscribe methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	payloadDict := Dictionary new.! !

!MQTTPacketSubscribe methodsFor: 'accessing' stamp: ''!
fixedHeaderFlags
	^ 2.! !

!MQTTPacketSubscribe methodsFor: 'accessing' stamp: ''!
packetType
	^ 8.! !

!MQTTPacketSubscribe methodsFor: 'accessing' stamp: ''!
subscriptionIdentifier
	^ subscriptionIdentifier.! !

!MQTTPacketSubscribe methodsFor: 'accessing' stamp: ''!
subscriptionIdentifier: arg1
	subscriptionIdentifier := arg1.! !

!MQTTPacketSubscribe methodsFor: 'accessing' stamp: ''!
topics
	^ payloadDict keys.! !

!MQTTPacketSubscribe methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handleSubscribePacket: self.! !

!MQTTPacketSubscribe methodsFor: 'mqtt' stamp: ''!
pendingJob
	^ MQTTPendingSubAckJob for: self.! !

!MQTTPacketSubscribe methodsFor: 'subscription' stamp: ''!
addSubscription: arg1
	payloadDict
		at: arg1 topic
		put: arg1.! !

!MQTTPacketSubscribe methodsFor: 'subscription' stamp: ''!
addTopic: arg1
	self
		addTopic: arg1
		qos: 0.! !

!MQTTPacketSubscribe methodsFor: 'subscription' stamp: ''!
addTopic: arg1 qos: arg2
	| temp3 |
	temp3 := MQTTSubscription new
		topic: arg1;
		qos: arg2;
		yourself.
	payloadDict
		at: arg1
		put: temp3.! !

!MQTTPacketSubscribe methodsFor: 'subscription' stamp: ''!
addTopic: arg1 qos: arg2 noLocal: arg3 retainAsPublished: arg4 retainHandling: arg5
	| temp6 |
	temp6 := MQTTSubscription new
		topic: arg1;
		qos: arg2;
		noLocal: arg3;
		retainAsPublished: arg4;
		retainHandling: arg5;
		yourself.
	payloadDict
		at: arg1
		put: temp6.! !

!MQTTPacketSubscribe methodsFor: 'encoding' stamp: ''!
decodePayloadFrom: arg1 length: arg2
	| temp3 temp4 temp5 temp6 |
	temp3 := arg1 position + arg2.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := self decodeStringFrom: arg1.
		temp5 := arg1 next.
		temp6 := MQTTSubscription new
			topic: temp4;
			qos: (temp5 bitAnd: 3);
			noLocal: (temp5 bitAt: 3) = 1;
			retainAsPublished: (temp5 bitAt: 4) = 1;
			retainHandling: ((temp5 bitShift: -4) bitAnd: 3);
			yourself.
		payloadDict
			at: temp4
			put: temp6 ].! !

!MQTTPacketSubscribe methodsFor: 'encoding' stamp: ''!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 11
			ifTrue: [ subscriptionIdentifier := self decodeVariableByteIntegerFrom: arg1 ]
			ifFalse: [
				temp4 = 38 ifTrue: [
					self decodeStringFrom: arg1.
					self decodeStringFrom: arg1 ]]].! !

!MQTTPacketSubscribe methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	self decodePropertiesFrom: arg1.! !

!MQTTPacketSubscribe methodsFor: 'encoding' stamp: ''!
encodePayloadOn: arg1
	payloadDict keysAndValuesDo: [ :argm0_2 :argm0_3 |
		self
			encodeString: argm0_2
			on: arg1.
		arg1 nextPut: argm0_3 subscriptionOptionsV5 ].! !

!MQTTPacketSubscribe methodsFor: 'encoding' stamp: ''!
encodePropertiesOn: arg1
	| temp2 |
	(subscriptionIdentifier isNil and: [
		properties isNil or: [ properties isEmpty ]]) ifTrue: [
		arg1 nextPut: 0.
		^ self ].
	temp2 := MQTTWriteStream on: (ByteArray new: 32).
	subscriptionIdentifier ifNotNil: [
		temp2 nextPut: 11.
		self
			encodeVariableByteInteger: subscriptionIdentifier
			on: temp2 ].
	properties ifNotNil: [
		properties do: [ :argm3_3 |
			argm3_3 encodeOn: temp2 ]].
	self
		encodeVariableByteInteger: temp2 position
		on: arg1.
	arg1 nextPutAll: temp2 contents.! !

!MQTTPacketSubscribe methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	self encodePropertiesOn: arg1.! !

!MQTTPacketUnsubscribe methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	topics := OrderedCollection new.! !

!MQTTPacketUnsubscribe methodsFor: 'accessing' stamp: ''!
fixedHeaderFlags
	^ 2.! !

!MQTTPacketUnsubscribe methodsFor: 'accessing' stamp: ''!
packetType
	^ 10.! !

!MQTTPacketUnsubscribe methodsFor: 'accessing' stamp: ''!
topics
	^ topics.! !

!MQTTPacketUnsubscribe methodsFor: 'accessing' stamp: ''!
topics: arg1
	topics := arg1.! !

!MQTTPacketUnsubscribe methodsFor: 'subscription' stamp: ''!
addTopic: arg1
	topics add: arg1.! !

!MQTTPacketUnsubscribe methodsFor: 'mqtt' stamp: ''!
evaluateFor: arg1
	arg1 handleUnsubscribePacket: self.! !

!MQTTPacketUnsubscribe methodsFor: 'mqtt' stamp: ''!
pendingJob
	^ MQTTPendingUnsubAckJob for: self.! !

!MQTTPacketUnsubscribe methodsFor: 'encoding' stamp: ''!
decodePayloadFrom: arg1 length: arg2
	| temp3 |
	topics := OrderedCollection new.
	temp3 := arg1 position + arg2.
	[ arg1 position < temp3 ] whileTrue: [ topics add: (self decodeStringFrom: arg1) ].! !

!MQTTPacketUnsubscribe methodsFor: 'encoding' stamp: ''!
decodePropertiesFrom: arg1
	| temp2 temp3 temp4 |
	temp2 := self decodeVariableByteIntegerFrom: arg1.
	temp2 = 0 ifTrue: [ ^ self ].
	temp3 := arg1 position + temp2.
	properties := MQTT5Properties new.
	[ arg1 position < temp3 ] whileTrue: [
		temp4 := arg1 next.
		temp4 = 38 ifTrue: [
			properties addProperty:
				(MQTT5Property
					identifier: temp4
					value:
						(Array
							with: (self decodeStringFrom: arg1)
							with: (self decodeStringFrom: arg1))) ]].! !

!MQTTPacketUnsubscribe methodsFor: 'encoding' stamp: ''!
decodeVariableHeaderFrom: arg1
	msgID := self decode16BitIntegerFrom: arg1.
	self decodePropertiesFrom: arg1.! !

!MQTTPacketUnsubscribe methodsFor: 'encoding' stamp: ''!
encodePayloadOn: arg1
	topics do: [ :argm0_2 |
		self
			encodeString: argm0_2
			on: arg1 ].! !

!MQTTPacketUnsubscribe methodsFor: 'encoding' stamp: ''!
encodePropertiesOn: arg1
	(properties isNil or: [ properties isEmpty ]) ifTrue: [
		arg1 nextPut: 0.
		^ self ].
	properties encodeOn: arg1.! !

!MQTTPacketUnsubscribe methodsFor: 'encoding' stamp: ''!
encodeVariableHeaderOn: arg1
	self
		encode16BitInteger: msgID
		on: arg1.
	self encodePropertiesOn: arg1.! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing' stamp: ''!
packet
	^ packet.! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing' stamp: ''!
packet: arg1
	packet := arg1.! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing' stamp: ''!
pendingJob
	^ pendingJob.! !

!MQTTPacketAndPendingJobPair methodsFor: 'accessing' stamp: ''!
pendingJob: arg1
	pendingJob := arg1.! !

!MQTTPacketAndPendingJobPair class methodsFor: 'instance creation' stamp: ''!
packet: arg1 job: arg2
	^ self new
		packet: arg1;
		pendingJob: arg2;
		yourself.! !

!MQTTPendingJob methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	sendTime := Time totalSeconds.! !

!MQTTPendingJob methodsFor: 'accessing' stamp: ''!
originalPacket
	^ originalPacket.! !

!MQTTPendingJob methodsFor: 'accessing' stamp: ''!
originalPacket: arg1
	originalPacket := arg1.! !

!MQTTPendingJob methodsFor: 'accessing' stamp: ''!
packetID
	^ originalPacket ifNotNil: [ originalPacket packetID ].! !

!MQTTPendingJob methodsFor: 'accessing' stamp: ''!
sendTime
	^ sendTime.! !

!MQTTPendingJob methodsFor: 'accessing' stamp: ''!
sendTime: arg1
	sendTime := arg1.! !

!MQTTPendingJob methodsFor: 'testing' stamp: ''!
isExpired: arg1
	^ Time totalSeconds - sendTime > arg1.! !

!MQTTPendingJob methodsFor: 'testing' stamp: ''!
isOverdue
	^ self isExpired: 30.! !

!MQTTPendingJob methodsFor: 'actions' stamp: ''!
resendOn: arg1
	originalPacket ifNotNil: [
		sendTime := Time totalSeconds.
		arg1 sendPacket: originalPacket ].! !

!MQTTPendingJob class methodsFor: 'instance creation' stamp: ''!
for: arg1
	^ self new
		originalPacket: arg1;
		yourself.! !

!MQTTSocketClient methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	cleanSessionFlag := true.
	willQos := 0.
	keepAliveTime := 60.
	protocolVersion := 5.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
cleanSessionFlag
	^ cleanSessionFlag.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
cleanSessionFlag: arg1
	cleanSessionFlag := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
clientID: arg1
	pendingClientID := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
interface
	^ interface.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
interface: arg1
	interface := arg1.
	transport := interface socketClient ifNotNil: [ interface socketClient transport ].! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
keepAliveTime
	^ keepAliveTime.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
keepAliveTime: arg1
	keepAliveTime := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
passwordString
	^ passwordString.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
passwordString: arg1
	passwordString := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
pendingSocket
	^ pendingSocket.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
pendingSocket: arg1
	pendingSocket := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
protocolVersion
	^ protocolVersion.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
protocolVersion: arg1
	protocolVersion := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
transport
	^ transport.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
transport: arg1
	transport := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
userNameString
	^ userNameString.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
userNameString: arg1
	userNameString := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
willMessage
	^ willMessage.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
willMessage: arg1
	willMessage := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
willQos
	^ willQos.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
willQos: arg1
	willQos := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
willRetainFlag
	^ willRetainFlag.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
willRetainFlag: arg1
	willRetainFlag := arg1.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
willTopic
	^ willTopic.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: ''!
willTopic: arg1
	willTopic := arg1.! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
acceptFrom: arg1
	self debugLog: 'MQTTSocketClient saw connectedOn: on socket'.
	transport isNil
		ifTrue: [
			transport := MQTTTransportLayerClient new.
			transport keepAliveTime: self keepAliveTime.
			transport socketClient: self.
			transport protocolVersion: self protocolVersion.
			pendingClientID ifNotNil: [ transport clientID: pendingClientID ].
			transport start: arg1.
			self initializeMQTTConnection: true ]
		ifFalse: [
			transport restart: arg1.
			self initializeMQTTConnection: false ].! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
close
	self debugLog: 'MQTTSocketClient saw close on socket'.
	self transport ifNotNil: [ self transport disconnect: true ].
	self release.! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
connect
	pendingSocket ifNil: [ self error: 'No pending socket - was openOnHostName called?' ].
	self acceptFrom: pendingSocket.
	pendingSocket := nil.! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
debugLog: arg1
	MQTTClientInterface
		debugLog: self printString
		tag: ' SC '
		str2: arg1.! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
disconnect
	self transport ifNotNil: [ self transport disconnect: false ].! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
initializeMQTTConnection: arg1
	| temp2 temp3 |
	temp2 := MQTTPacketConnect new keepAliveTime: self keepAliveTime.
	willTopic isString & willMessage isString & (willRetainFlag isKindOf: Boolean) & willQos isInteger ifTrue: [
		temp2
			willTopic: willTopic
			message: willMessage
			retain: willRetainFlag
			qos: willQos ].
	cleanSessionFlag := arg1.
	temp2 cleanSessionFlag: cleanSessionFlag.
	temp2
		user: userNameString
		password: passwordString.
	temp2 clientID: self transport clientIDString.
	self protocolVersion = 5 ifTrue: [ temp2 protocolLevel: 5 ].
	arg1
		ifTrue: [
			self transport outgoingPacketQueue: SharedQueue new.
			self transport packetInFlightQueue: SharedQueue new.
			self transport sendPacket: temp2 ]
		ifFalse: [
			temp3 := SharedQueue new.
			temp3 nextPut: temp2.
			temp3 nextPutAll: self transport packetInFlightQueue contents.
			self transport packetInFlightQueue: temp3 ].! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
onTopic: arg1 qos: arg2 do: arg3
	self transport
		onTopic: arg1
		qos: arg2
		do: arg3.! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
publishTopic: arg1 message: arg2 qos: arg3 retain: arg4
	self transport
		publishTopic: arg1
		message: arg2
		qos: arg3
		retain: arg4.! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
readWaitTime: arg1
	self transport readWaitTime: arg1.! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
release
	interface := nil.
	transport := nil.! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
unsubscribeFrom: arg1
	self transport unsubscribeFrom: arg1.! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
username: arg1 password: arg2
	arg1 ifNotNil: [ userNameString := UTF8Encoder encode: arg1 asString ].
	arg2 ifNotNil: [ passwordString := UTF8Encoder encode: arg2 asString ].! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: ''!
willTopic: arg1 message: arg2 retain: arg3 qos: arg4
	arg1 isString & arg2 isString & (arg3 = true | arg3 = false) &
		(arg4 isInteger &
			(arg4
				between: 0
				and: 2)) ifFalse: [ ^ self error: 'Bad Will' ].
	willTopic := arg1.
	willMessage := arg2.
	willRetainFlag := arg3.
	willQos := arg4.! !

!MQTTSocketClient class methodsFor: 'instance creation' stamp: ''!
openOnHostName: arg1 port: arg2 keepAlive: arg3 interface: arg4
	| temp5 temp6 temp7 |
	temp7 := NetNameResolver
		addressForName: arg1
		timeout: arg3.
	temp5 := self new.
	temp6 := Socket newTCP.
	temp6
		connectTo: temp7
		port: arg2.
	temp6 waitForConnectionFor: arg3.
	temp5 keepAliveTime: arg3.
	temp5 interface: arg4.
	temp5 pendingSocket: temp6.
	^ temp5.! !

!MQTTStatistics methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	pin := 0.
	pout := 0.
	pinBytes := 0.
	poutBytes := 0.
	pinMaxSize := 0.
	pinMinSize := 99999999.
	poutMaxSize := 0.
	poutMinSize := 99999999.
	pinTypeCount := Dictionary new.
	poutTypeCount := Dictionary new.
	errorTypes := Dictionary new.
	connections := 0.
	disconnect := 0.
	connected := false.
	connectionTypes := Dictionary new.
	servers := Dictionary new.
	subscriptions := 0.
	timeOut := 0.
	killSocket := 0.
	sentPackets := 0.
	queuedPackets := 0.
	unsubscribe := 0.
	badConnections := 0.
	missingType := 0.
	resendType := 0.! !

!MQTTStatistics methodsFor: 'accessing' stamp: ''!
pin
	^ pin.! !

!MQTTStatistics methodsFor: 'accessing' stamp: ''!
pinBytes
	^ pinBytes.! !

!MQTTStatistics methodsFor: 'accessing' stamp: ''!
pout
	^ pout.! !

!MQTTStatistics methodsFor: 'accessing' stamp: ''!
poutBytes
	^ poutBytes.! !

!MQTTStatistics methodsFor: 'updating' stamp: ''!
recordConnection
	connections := connections + 1.
	connected := true.! !

!MQTTStatistics methodsFor: 'updating' stamp: ''!
recordDisconnection
	disconnect := disconnect + 1.
	connected := false.! !

!MQTTStatistics methodsFor: 'updating' stamp: ''!
recordError: arg1
	errorTypes
		at: arg1
		put:
			(errorTypes
				at: arg1
				ifAbsent: [ 0 ]) + 1.! !

!MQTTStatistics methodsFor: 'updating' stamp: ''!
recordIncomingPacket: arg1 size: arg2
	pin := pin + 1.
	pinBytes := pinBytes + arg2.
	pinMaxSize := pinMaxSize max: arg2.
	pinMinSize := pinMinSize min: arg2.
	pinTypeCount
		at: arg1 class name
		put:
			(pinTypeCount
				at: arg1 class name
				ifAbsent: [ 0 ]) + 1.! !

!MQTTStatistics methodsFor: 'updating' stamp: ''!
recordOutgoingPacket: arg1 size: arg2
	pout := pout + 1.
	poutBytes := poutBytes + arg2.
	poutMaxSize := poutMaxSize max: arg2.
	poutMinSize := poutMinSize min: arg2.
	poutTypeCount
		at: arg1 class name
		put:
			(poutTypeCount
				at: arg1 class name
				ifAbsent: [ 0 ]) + 1.! !

!MQTTSubscription methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	qos := 0.
	noLocal := false.
	retainAsPublished := false.
	retainHandling := 0.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
actionBlock
	^ actionBlock.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
actionBlock: arg1
	actionBlock := arg1.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
noLocal
	^ noLocal.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
noLocal: arg1
	noLocal := arg1.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
qos
	^ qos.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
qos: arg1
	qos := arg1.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
retainAsPublished
	^ retainAsPublished.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
retainAsPublished: arg1
	retainAsPublished := arg1.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
retainFlag
	^ retainFlag.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
retainFlag: arg1
	retainFlag := arg1.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
retainHandling
	^ retainHandling.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
retainHandling: arg1
	retainHandling := arg1.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
topic
	^ topic.! !

!MQTTSubscription methodsFor: 'accessing' stamp: ''!
topic: arg1
	topic := arg1.! !

!MQTTSubscription methodsFor: 'encoding' stamp: ''!
subscriptionOptionsV311
	^ qos bitAnd: 3.! !

!MQTTSubscription methodsFor: 'encoding' stamp: ''!
subscriptionOptionsV5
	| temp1 |
	temp1 := qos bitAnd: 3.
	noLocal ifTrue: [ temp1 := temp1 bitOr: 4 ].
	retainAsPublished ifTrue: [ temp1 := temp1 bitOr: 8 ].
	temp1 := temp1 bitOr: ((retainHandling bitAnd: 3) bitShift: 4).
	^ temp1.! !

!MQTTSubscription methodsFor: 'matching' stamp: ''!
matchParts: arg1 against: arg2 index: arg3
	| temp4 temp5 |
	arg3 > arg1 size ifTrue: [ ^ arg3 > arg2 size ].
	temp4 := arg1 at: arg3.
	temp4 = '#' ifTrue: [ ^ true ].
	arg3 > arg2 size ifTrue: [ ^ false ].
	temp5 := arg2 at: arg3.
	(temp4 = '+' or: [ temp4 = temp5 ]) ifTrue: [ ^ self
			matchParts: arg1
			against: arg2
			index: arg3 + 1 ].
	^ false.! !

!MQTTSubscription methodsFor: 'matching' stamp: ''!
topicMatches: arg1
	| temp2 temp3 |
	topic = arg1 ifTrue: [ ^ true ].
	((topic includes: $+) or: [ topic includes: $# ]) ifFalse: [ ^ false ].
	temp2 := topic findTokens: $/.
	temp3 := arg1 findTokens: $/.
	^ self
		matchParts: temp2
		against: temp3
		index: 1.! !

!MQTTSubscription class methodsFor: 'instance creation' stamp: ''!
for: arg1 qos: arg2 do: arg3
	^ self new
		topic: arg1;
		qos: arg2;
		actionBlock: arg3;
		yourself.! !

!MQTTTransportLayer methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	abort := false.
	retryTime := 30.
	readWaitTime := 0.
	lastMID := 0.
	packetInFlightQueue := SharedQueue new.
	outgoingPacketQueue := SharedQueue new.
	pendingJobs := Dictionary new.
	currentSubscriptions := Dictionary new.
	pendingJobsMutex := Semaphore forMutualExclusion.
	abortMutex := Semaphore forMutualExclusion.
	statisticsMutex := Semaphore forMutualExclusion.
	statistics := MQTTStatistics new.
	protocolVersion := 5.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
abort
	^ abort.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
abort: arg1
	abortMutex critical: [ abort := arg1 ].! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
isVersion5
	^ protocolVersion = 5.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
outgoingPacketQueue
	^ outgoingPacketQueue.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
outgoingPacketQueue: arg1
	outgoingPacketQueue := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
packetInFlightQueue
	^ packetInFlightQueue.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
packetInFlightQueue: arg1
	packetInFlightQueue := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
protocolVersion
	^ protocolVersion.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
protocolVersion: arg1
	protocolVersion := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
readWaitTime
	^ readWaitTime.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
readWaitTime: arg1
	readWaitTime := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
retryTime
	^ retryTime.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
retryTime: arg1
	retryTime := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
sockStrm
	^ sockStrm.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
sockStrm: arg1
	sockStrm := arg1.! !

!MQTTTransportLayer methodsFor: 'accessing' stamp: ''!
statistics
	^ statistics.! !

!MQTTTransportLayer methodsFor: 'message ids' stamp: ''!
nextMsgID
	lastMID := lastMID \\ 65535 + 1.
	^ lastMID.! !

!MQTTTransportLayer methodsFor: 'subscriptions' stamp: ''!
addSubscription: arg1
	currentSubscriptions
		at: arg1 topic
		put: arg1.! !

!MQTTTransportLayer methodsFor: 'subscriptions' stamp: ''!
currentSubscriptions
	^ currentSubscriptions.! !

!MQTTTransportLayer methodsFor: 'subscriptions' stamp: ''!
removeSubscription: arg1
	currentSubscriptions
		removeKey: arg1
		ifAbsent: [ ].! !

!MQTTTransportLayer methodsFor: 'pending jobs' stamp: ''!
addPendingJob: arg1 forPacketID: arg2
	pendingJobsMutex critical: [
		pendingJobs
			at: arg2
			put: arg1 ].! !

!MQTTTransportLayer methodsFor: 'pending jobs' stamp: ''!
pendingJobForPacketID: arg1
	^ pendingJobsMutex critical: [
		pendingJobs
			at: arg1
			ifAbsent: [ ]].! !

!MQTTTransportLayer methodsFor: 'pending jobs' stamp: ''!
removePendingJobForPacketID: arg1
	pendingJobsMutex critical: [
		pendingJobs
			removeKey: arg1
			ifAbsent: [ ]].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
badTopicError
	MQTTBadTopicException signal.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
debugLog: arg1
	MQTTClientInterface
		debugLog: self printString
		tag: ' TL '
		str2: arg1.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
disconnect: arg1
	(self preambleWorkAbortEarlyIfTrue: arg1) ifTrue: [ ^ self ].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
disconnectPostCleanup
	sockStrm ifNotNil: [
		[ sockStrm close ]
			on: Error
			do: [ :argm0_1 |
				nil ].
		sockStrm := nil ].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
doSubscriptionActionsFor: arg1
	currentSubscriptions valuesDo: [ :argm0_2 |
		arg1
			matchesSubscription: argm0_2
			ifTrue: argm0_2 actionBlock ].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
exceptionHandler: arg1
	self debugLog: 'Exception: ' , arg1 printString.
	self
		statPerform: #errorTypes:
		with: arg1 class name.
	arg1 class = MQTTEmptyStreamErrorException ifTrue: [ (Delay forMilliseconds: 100) wait ].
	^ nil.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
findAndRunJobsOverdueForAcknowledgement
	| temp1 |
	temp1 := OrderedCollection new.
	pendingJobsMutex critical: [
		pendingJobs keysAndValuesDo: [ :argm1_2 :argm1_3 |
			argm1_3 isOverdue ifTrue: [ temp1 add: argm1_3 ]]].
	temp1 do: [ :argm2_2 |
		self debugLog: 'Resending packet for job: ' , argm2_2 printString.
		argm2_2 resendOn: self ].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handleConnAckPacket: arg1
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handleDisconnect: arg1
	self debugLog: 'Received DISCONNECT from server'.
	self disconnect: false.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handleIncomingPacket: arg1
	| temp2 |
	temp2 := arg1 packetType.
	temp2 = 2 ifTrue: [ ^ self handleConnAckPacket: arg1 ].
	temp2 = 3 ifTrue: [ ^ self
			handlePublishPacket: arg1
			qos: arg1 qos ].
	temp2 = 4 ifTrue: [ ^ self handlePubAckPacket: arg1 ].
	temp2 = 5 ifTrue: [ ^ self handlePubRecPacket: arg1 ].
	temp2 = 6 ifTrue: [ ^ self handlePubRelPacket: arg1 ].
	temp2 = 7 ifTrue: [ ^ self handlePubCompPacket: arg1 ].
	temp2 = 9 ifTrue: [ ^ self handleSubAckPacket: arg1 ].
	temp2 = 11 ifTrue: [ ^ self handleUnsubAckPacket: arg1 ].
	temp2 = 13 ifTrue: [ ^ self handlePingResp ].
	temp2 = 14 ifTrue: [ ^ self handleDisconnect: arg1 ].
	self debugLog: 'Unknown packet type: ' , temp2 printString.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handlePingResp
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handlePubAckPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #pubAck ].
	self removePendingJobForPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handlePubCompPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #pubComp ].
	self removePendingJobForPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handlePubRecPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #pubRec ].
	self sendPacket: arg1 acknowledgement.
	self removePendingJobForPacketID: arg1 packetID.
	self
		addPendingJob: (MQTTPendingPubCompJob for: arg1)
		forPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handlePubRelPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ self missingPendingJob: #pubRel ].
	self sendPacket: arg1 acknowledgement.
	temp2 ifNotNil: [
		self removePendingJobForPacketID: arg1 packetID.
		self doSubscriptionActionsFor: temp2 originalPacket ].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handlePublishPacket: arg1 qos: arg2
	| temp3 temp4 |
	arg2 >= 1 ifTrue: [
		temp3 := arg1 acknowledgement.
		arg2 = 1 ifTrue: [ self sendPacket: temp3 ]].
	arg2 = 2 ifTrue: [
		(self pendingJobForPacketID: arg1 packetID) ifNotNil: [ ^ self ].
		temp4 := MQTTPendingPubRelJob for: arg1.
		self
			addPendingJob: temp4
			forPacketID: arg1 packetID.
		self sendPacket:
			(MQTTPacketAndPendingJobPair
				packet: temp3
				job: temp4).
		^ self ].
	self doSubscriptionActionsFor: arg1.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handleSubAckPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #subAck ].
	self removePendingJobForPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
handleUnsubAckPacket: arg1
	| temp2 |
	temp2 := self pendingJobForPacketID: arg1 packetID.
	temp2 ifNil: [ ^ self missingPendingJob: #unSubAck ].
	self removePendingJobForPacketID: arg1 packetID.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
initializeKeepAlive
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
initializeMQTTConnection: arg1
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
initializePacketReading
	| temp1 temp2 |
	temp1 := self packetReader.
	incomingPacketProcess := [
	[
	temp2 := nil.
	temp2 := [ temp1 value ]
		on: Error
		do: [ :argm6_4 |
			self exceptionHandler: argm6_4 ].
	temp2 ifNotNil: [
		self
			statPerform: #packetIn:
			with: temp2.
		(temp2 packetType = 3 and: [ temp2 qos > 0 ]) ifTrue: [ self debugLog: 'Rx ' , temp2 asString ].
		[ self handleIncomingPacket: temp2 ]
			on: Error
			do: [ :argm9_4 |
				self exceptionHandler: argm9_4 ]].
	Processor yield.
	self abort ] whileFalse ] fork.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
initializePacketWriting
	| temp1 temp2 |
	temp1 := self packetWriter.
	outgoingPacketProcess := [
	[
	temp2 := nil.
	temp2 := self packetInFlightQueue nextOrNil.
	temp2 ifNil: [ temp2 := self outgoingPacketQueue next ].
	self packetInFlightQueue nextPut: temp2.
	self outgoingPacketQueue size > 10 ifTrue: [ self debugLog: 'write packet backlog at: ' , self outgoingPacketQueue size printString ].
	[ temp1 value: temp2 ]
		on: Error
		do: [ :argm14_4 |
			self exceptionHandler: argm14_4 ].
	self abort ifFalse: [ self packetInFlightQueue nextOrNil ].
	Processor yield.
	self abort ] whileFalse ] fork.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
initializeRetryProcess
	retryProcess := [ | temp0_1 |
	temp0_1 := Delay forSeconds: 2.
	[
	self abort ifFalse: [ self findAndRunJobsOverdueForAcknowledgement ].
	temp0_1 wait.
	self abort ] whileFalse ] fork.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
initializeSocketStream: arg1
	sockStrm := SocketStream on: arg1.
	sockStrm binary.
	sockStrm timeout: self readWaitTime.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
missingPendingJob: arg1
	self debugLog: '************ No matching pending job for type: ' , arg1 asString.
	self
		statPerform: #missingType:
		with: arg1.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
onTopic: arg1 qos: arg2 do: arg3
	| temp4 |
	temp4 := MQTTSubscription
		for: arg1
		qos: arg2
		do: arg3.
	temp4 ifNil: [ ^ self badTopicError ].
	self addSubscription: temp4.
	self
		subscribe: arg1
		qos: arg2.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
packetReader
	^ [ MQTTPacket readFrom: sockStrm ].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
packetWriter
	^ [ :argm0_1 | | temp0_2 |
	temp0_2 := (argm0_1 isKindOf: MQTTPacketAndPendingJobPair)
		ifTrue: [ argm0_1 packet ]
		ifFalse: [ argm0_1 ].
	self debugLog: 'Wx ' , temp0_2 asString.
	temp0_2 encodeOn: sockStrm.
	self
		statPerform: #packetOut:
		with: temp0_2 ].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
preambleWorkAbortEarlyIfTrue: arg1
	abortMutex critical: [
		abort ifTrue: [ ^ true ].
		self debugLog: 'disconnecting with restart: ' , arg1 printString.
		self statPerform: #disconnect.
		self sendPossibleDisconnectPackage.
		abort := true.
		self disconnectPostCleanup ].
	^ false.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
publishTopic: arg1 message: arg2 qos: arg3 retain: arg4
	| temp5 temp6 |
	temp5 := MQTTPacketPublish new
		
			topic: arg1
			message: arg2 asByteArray;
		retainFlag: arg4;
		qos: arg3.
	arg3 > 0 ifTrue: [
		temp5 messageID: self nextMsgID.
		temp6 := temp5 pendingJob.
		self
			addPendingJob: temp6
			forPacketID: temp5 messageID ].
	self sendPacket:
		(MQTTPacketAndPendingJobPair
			packet: temp5
			job: temp6).! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
restart: arg1
	(Delay forSeconds: 3) wait.
	self debugLog: 'reStart Socket'.
	abortMutex critical: [ abort := false ].
	self initializeSocketStream: arg1.
	self
		initializeMQTTConnection: false;
		initializePacketReading;
		initializePacketWriting;
		initializeRetryProcess.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
sendPacket: arg1
	outgoingPacketQueue nextPut: arg1.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
sendPossibleDisconnectPackage
	^ self.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
start: arg1
	self debugLog: 'Start Socket'.
	abort := false.
	retryTime := 20.
	readWaitTime := 30.
	pendingJobsMutex := Semaphore forMutualExclusion.
	currentSubscriptions := Dictionary new.
	outgoingPacketQueue := SharedQueue new.
	packetInFlightQueue := SharedQueue new.
	statistics := MQTTStatistics new.
	statisticsMutex := Semaphore forMutualExclusion.
	abortMutex := Semaphore forMutualExclusion.
	self initializeSocketStream: arg1.
	self
		initializeMQTTConnection: true;
		initializePacketReading;
		initializePacketWriting;
		initializeRetryProcess.
	self debugLog: 'Started Socket'.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
statPerform: arg1
	statisticsMutex critical: [
		[ statistics perform: arg1 ]
			on: Error
			do: [ :argm0_2 |
				nil ]].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
statPerform: arg1 with: arg2
	statisticsMutex critical: [
		[
		statistics
			perform: arg1
			with: arg2 ]
			on: Error
			do: [ :argm0_3 |
				nil ]].! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
subscribe: arg1 qos: arg2
	| temp3 temp4 |
	temp3 := MQTTPacketSubscribe new
		
			addTopic: arg1
			qos: arg2;
		packetID: self nextMsgID.
	temp4 := temp3 pendingJob.
	self
		addPendingJob: temp4
		forPacketID: temp3 packetID.
	self sendPacket:
		(MQTTPacketAndPendingJobPair
			packet: temp3
			job: temp4).! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
transportType
	^ ' TL '.! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: ''!
unsubscribeFrom: arg1
	| temp2 temp3 |
	self removeSubscription: arg1.
	temp2 := MQTTPacketUnsubscribe new
		addTopic: arg1;
		packetID: self nextMsgID.
	temp3 := temp2 pendingJob.
	self
		addPendingJob: temp3
		forPacketID: temp2 packetID.
	self sendPacket:
		(MQTTPacketAndPendingJobPair
			packet: temp2
			job: temp3).! !

!MQTTTransportLayerClient methodsFor: 'initialization' stamp: ''!
initialize
	super initialize.
	pingOK := false.
	keepAliveTime := 60.
	serverCapabilities := MQTT5ServerCapabilities new.
	topicAliasMapOutgoing := Dictionary new.
	topicAliasMapIncoming := Dictionary new.! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: ''!
clientID
	^ clientID.! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: ''!
clientID: arg1
	clientID := arg1.! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: ''!
clientIDString
	^ clientID ifNil: [ clientID := self class nextClientID ].! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: ''!
keepAliveTime
	^ keepAliveTime.! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: ''!
keepAliveTime: arg1
	keepAliveTime := arg1.! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: ''!
pingOK
	^ pingOK.! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: ''!
pingOK: arg1
	pingOK := arg1.! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: ''!
socketClient
	^ socketClient.! !

!MQTTTransportLayerClient methodsFor: 'accessing' stamp: ''!
socketClient: arg1
	socketClient := arg1.! !

!MQTTTransportLayerClient methodsFor: 'v5 capabilities' stamp: ''!
serverCapabilities
	^ serverCapabilities.! !

!MQTTTransportLayerClient methodsFor: 'v5 capabilities' stamp: ''!
serverCapabilities: arg1
	serverCapabilities := arg1.! !

!MQTTTransportLayerClient methodsFor: 'v5 capabilities' stamp: ''!
updateServerCapabilities: arg1
	self isVersion5 ifFalse: [ ^ self ].
	arg1 properties ifNotNil: [ :temp2 |
		serverCapabilities updateFromConnAckProperties: temp2 ].
	serverCapabilities assignedClientIdentifier ifNotNil: [ :temp3 |
		clientID := temp3 ].! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias' stamp: ''!
clearTopicAliasMaps
	topicAliasMapOutgoing := Dictionary new.
	topicAliasMapIncoming := Dictionary new.! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias' stamp: ''!
nextTopicAliasFor: arg1
	| temp2 temp3 temp4 |
	self isVersion5 ifFalse: [ ^ nil ].
	temp2 := serverCapabilities topicAliasMaximum.
	temp2 = 0 ifTrue: [ ^ nil ].
	temp3 := topicAliasMapOutgoing
		keyAtValue: arg1
		ifAbsent: [ ].
	temp3 ifNotNil: [ ^ temp3 ].
	topicAliasMapOutgoing size < temp2 ifTrue: [
		temp4 := topicAliasMapOutgoing size + 1.
		topicAliasMapOutgoing
			at: temp4
			put: arg1.
		^ temp4 ].
	^ nil.! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias' stamp: ''!
registerTopicAliasIncoming: arg1 forTopic: arg2
	topicAliasMapIncoming
		at: arg1
		put: arg2.! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias' stamp: ''!
resolveTopicAliasIncoming: arg1
	^ topicAliasMapIncoming
		at: arg1
		ifAbsent: [ ].! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias' stamp: ''!
topicAliasMapIncoming
	^ topicAliasMapIncoming.! !

!MQTTTransportLayerClient methodsFor: 'v5 topic alias' stamp: ''!
topicAliasMapOutgoing
	^ topicAliasMapOutgoing.! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: ''!
disconnect: arg1
	(self preambleWorkAbortEarlyIfTrue: arg1) ifTrue: [ ^ self ].
	self clearTopicAliasMaps.
	arg1 ifTrue: [
		self socketClient ifNotNil: [ :temp2 |
			temp2 interface restart ]].! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: ''!
handleConnAckPacket: arg1
	| temp2 |
	temp2 := arg1 returnCode ifNil: [ arg1 reasonCode ifNil: [ 0 ]].
	temp2 = 0 ifFalse: [
		self debugLog: 'CONNACK failed with code: ' , temp2 printString.
		MQTTCONNECTException connectReturnCode: temp2 ].
	self isVersion5 ifTrue: [ self updateServerCapabilities: arg1 ].
	self debugLog: 'CONNACK received - connection established'.
	pingOK := true.
	self initializeKeepAlive.! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: ''!
handlePingResp
	pingOK := true.
	self debugLog: 'PINGRESP received'.! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: ''!
initializeKeepAlive
	keepAliveTime > 0 ifFalse: [ ^ self ].
	[ | temp1_1 |
	temp1_1 := Delay forSeconds: (keepAliveTime // 2 max: 10).
	[
	pingOK ifTrue: [
		pingOK := false.
		self sendPacket: MQTTPacketPingReq new ].
	temp1_1 wait.
	self abort ] whileFalse ] fork.! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: ''!
sendPossibleDisconnectPackage
	[ | temp0_1 |
	temp0_1 := MQTTPacketDisconnect new.
	self isVersion5 ifTrue: [ temp0_1 reasonCode: 0 ].
	temp0_1 encodeOn: sockStrm ]
		on: Error
		do: [ :argm0_1 |
			nil ].! !

!MQTTTransportLayerClient methodsFor: 'mqtt' stamp: ''!
transportType
	^ ' TC '.! !

!MQTTTransportLayerClient class methodsFor: 'class initialization' stamp: ''!
initialize
	ClientID := 0.! !

!MQTTTransportLayerClient class methodsFor: 'accessing' stamp: ''!
nextClientID
	ClientID := ClientID + 1.
	^ 'MQTTClient_' , ClientID printString.! !

!UTF8Encoder class methodsFor: 'encoding' stamp: 'JMM 2/16/2026 02:58'!
decode: aByteArrayOrString
	"Decode UTF-8 bytes to a String. Squeak-compatible replacement for Cuis CharacterSequence fromUtf8Bytes:."
	| result stream byte codePoint |
	aByteArrayOrString isString ifTrue: [^ aByteArrayOrString].
	result := WriteStream on: (String new: aByteArrayOrString size).
	stream := ReadStream on: aByteArrayOrString.
	[stream atEnd] whileFalse: [
		byte := stream next.
		byte < 128
			ifTrue: [result nextPut: (Character value: byte)]
			ifFalse: [
				(byte bitAnd: 16rE0) = 16rC0
					ifTrue: [
						codePoint := ((byte bitAnd: 16r1F) bitShift: 6)
							bitOr: (stream next bitAnd: 16r3F).
						result nextPut: (Character value: codePoint)]
					ifFalse: [
						(byte bitAnd: 16rF0) = 16rE0
							ifTrue: [
								codePoint := ((byte bitAnd: 16r0F) bitShift: 12)
									bitOr: (((stream next bitAnd: 16r3F) bitShift: 6)
									bitOr: (stream next bitAnd: 16r3F)).
								result nextPut: (Character value: codePoint)]
							ifFalse: [
								"4-byte UTF-8"
								codePoint := ((byte bitAnd: 16r07) bitShift: 18)
									bitOr: (((stream next bitAnd: 16r3F) bitShift: 12)
									bitOr: (((stream next bitAnd: 16r3F) bitShift: 6)
									bitOr: (stream next bitAnd: 16r3F))).
								result nextPut: (Character value: codePoint)]]]].
	^ result contents! !

!UTF8Encoder class methodsFor: 'encoding' stamp: 'JMM 2/16/2026 02:58'!
encode: aString
	"Encode a String to UTF-8 bytes. Squeak-compatible replacement for Cuis asUtf8Bytes."
	| result cp |
	result := WriteStream on: (ByteArray new: aString size * 2).
	aString do: [:char |
		cp := char asInteger.
		cp < 128
			ifTrue: [result nextPut: cp]
			ifFalse: [
				cp < 16r800
					ifTrue: [
						result nextPut: (16rC0 bitOr: (cp bitShift: -6)).
						result nextPut: (16r80 bitOr: (cp bitAnd: 16r3F))]
					ifFalse: [
						cp < 16r10000
							ifTrue: [
								result nextPut: (16rE0 bitOr: (cp bitShift: -12)).
								result nextPut: (16r80 bitOr: ((cp bitShift: -6) bitAnd: 16r3F)).
								result nextPut: (16r80 bitOr: (cp bitAnd: 16r3F))]
							ifFalse: [
								result nextPut: (16rF0 bitOr: (cp bitShift: -18)).
								result nextPut: (16r80 bitOr: ((cp bitShift: -12) bitAnd: 16r3F)).
								result nextPut: (16r80 bitOr: ((cp bitShift: -6) bitAnd: 16r3F)).
								result nextPut: (16r80 bitOr: (cp bitAnd: 16r3F))]]]].
	^ result contents! !

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
	self
		assert: received
		equals: temp3.
	self
		assert: receivedTopic
		equals: temp2.! !

!False methodsFor: '*MQTT5-Squeak' stamp: ''!
asBit
	^ 0.! !

!True methodsFor: '*MQTT5-Squeak' stamp: ''!
asBit
	^ 1.! !

!Integer methodsFor: '*MQTT5-Squeak' stamp: ''!
bitAt: arg1 put: arg2
	arg2 = 1 ifTrue: [ ^ self bitOr: (1 bitShift: arg1 - 1) ].
	^ self bitAnd: (1 bitShift: arg1 - 1) bitInvert.! !
MQTTPacket initialize!
MQTTTransportLayerClient initialize!

!MQTTPacketPubAck methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
reasonString
	^ reasonString.! !

!MQTTPacketPubAck methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
reasonString: aString
	reasonString := aString.! !

!MQTTPacketPubAck methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
userProperties
	^ (properties ifNil: [ ^ #() ]) properties
		select: [ :p | p identifier = MQTT5Constants userProperty ]
		thenCollect: [ :p | p value ].! !

!MQTTPacketPubAck methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
addUserProperty: key value: val
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addPropertyIdentifier: MQTT5Constants userProperty value: (key -> val).! !

!MQTTPacketPubAck methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
hasProperties
	^ reasonString notNil or: [
		properties notNil and: [ properties notEmpty ] ].! !

!MQTTPacketPubAck methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
encodeVariableHeaderOn: arg1
	self encode16BitInteger: msgID on: arg1.
	(reasonCode ~= 0 or: [ self hasProperties ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].! !

!MQTTPacketPubAck methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
encodePropertiesOn: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	reasonString ifNotNil: [
		temp2 addPropertyIdentifier: MQTT5Constants reasonString value: reasonString ].
	properties ifNotNil: [
		properties do: [ :p |
			p identifier = MQTT5Constants userProperty ifTrue: [ temp2 add: p ] ] ].
	temp2 encodeOn: arg1.! !

!MQTTPacketPubAck methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
decodeFrom: arg1 remainingLength: arg2
	remainingLength := arg2.
	msgID := self decode16BitIntegerFrom: arg1.
	arg2 <= 2 ifTrue: [ ^ self ].
	reasonCode := arg1 next.
	arg2 <= 3 ifTrue: [ ^ self ].
	self decodePropertiesFrom: arg1.! !

!MQTTPacketPubAck methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
decodePropertiesFrom: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	temp2 decodeFrom: arg1.
	properties := temp2.
	(temp2 propertiesAt: MQTT5Constants reasonString) ifNotEmpty: [ :rs |
		reasonString := rs first value ].! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
reasonString
	^ reasonString.! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
reasonString: aString
	reasonString := aString.! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
userProperties
	^ (properties ifNil: [ ^ #() ]) properties
		select: [ :p | p identifier = MQTT5Constants userProperty ]
		thenCollect: [ :p | p value ].! !

!MQTTPacketPubRec methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
addUserProperty: key value: val
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addPropertyIdentifier: MQTT5Constants userProperty value: (key -> val).! !

!MQTTPacketPubRec methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
hasProperties
	^ reasonString notNil or: [
		properties notNil and: [ properties notEmpty ] ].! !

!MQTTPacketPubRec methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
encodeVariableHeaderOn: arg1
	self encode16BitInteger: msgID on: arg1.
	(reasonCode ~= 0 or: [ self hasProperties ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].! !

!MQTTPacketPubRec methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
encodePropertiesOn: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	reasonString ifNotNil: [
		temp2 addPropertyIdentifier: MQTT5Constants reasonString value: reasonString ].
	properties ifNotNil: [
		properties do: [ :p |
			p identifier = MQTT5Constants userProperty ifTrue: [ temp2 add: p ] ] ].
	temp2 encodeOn: arg1.! !

!MQTTPacketPubRec methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
decodeFrom: arg1 remainingLength: arg2
	remainingLength := arg2.
	msgID := self decode16BitIntegerFrom: arg1.
	arg2 <= 2 ifTrue: [ ^ self ].
	reasonCode := arg1 next.
	arg2 <= 3 ifTrue: [ ^ self ].
	self decodePropertiesFrom: arg1.! !

!MQTTPacketPubRec methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
decodePropertiesFrom: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	temp2 decodeFrom: arg1.
	properties := temp2.
	(temp2 propertiesAt: MQTT5Constants reasonString) ifNotEmpty: [ :rs |
		reasonString := rs first value ].! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
reasonString
	^ reasonString.! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
reasonString: aString
	reasonString := aString.! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
userProperties
	^ (properties ifNil: [ ^ #() ]) properties
		select: [ :p | p identifier = MQTT5Constants userProperty ]
		thenCollect: [ :p | p value ].! !

!MQTTPacketPubRel methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
addUserProperty: key value: val
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addPropertyIdentifier: MQTT5Constants userProperty value: (key -> val).! !

!MQTTPacketPubRel methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
hasProperties
	^ reasonString notNil or: [
		properties notNil and: [ properties notEmpty ] ].! !

!MQTTPacketPubRel methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
encodeVariableHeaderOn: arg1
	self encode16BitInteger: msgID on: arg1.
	(reasonCode ~= 0 or: [ self hasProperties ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].! !

!MQTTPacketPubRel methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
encodePropertiesOn: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	reasonString ifNotNil: [
		temp2 addPropertyIdentifier: MQTT5Constants reasonString value: reasonString ].
	properties ifNotNil: [
		properties do: [ :p |
			p identifier = MQTT5Constants userProperty ifTrue: [ temp2 add: p ] ] ].
	temp2 encodeOn: arg1.! !

!MQTTPacketPubRel methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
decodeFrom: arg1 remainingLength: arg2
	remainingLength := arg2.
	msgID := self decode16BitIntegerFrom: arg1.
	arg2 <= 2 ifTrue: [ ^ self ].
	reasonCode := arg1 next.
	arg2 <= 3 ifTrue: [ ^ self ].
	self decodePropertiesFrom: arg1.! !

!MQTTPacketPubRel methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
decodePropertiesFrom: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	temp2 decodeFrom: arg1.
	properties := temp2.
	(temp2 propertiesAt: MQTT5Constants reasonString) ifNotEmpty: [ :rs |
		reasonString := rs first value ].! !

!MQTTPacketPubComp methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
reasonString
	^ reasonString.! !

!MQTTPacketPubComp methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
reasonString: aString
	reasonString := aString.! !

!MQTTPacketPubComp methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
userProperties
	^ (properties ifNil: [ ^ #() ]) properties
		select: [ :p | p identifier = MQTT5Constants userProperty ]
		thenCollect: [ :p | p value ].! !

!MQTTPacketPubComp methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:20'!
addUserProperty: key value: val
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addPropertyIdentifier: MQTT5Constants userProperty value: (key -> val).! !

!MQTTPacketPubComp methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
hasProperties
	^ reasonString notNil or: [
		properties notNil and: [ properties notEmpty ] ].! !

!MQTTPacketPubComp methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
encodeVariableHeaderOn: arg1
	self encode16BitInteger: msgID on: arg1.
	(reasonCode ~= 0 or: [ self hasProperties ]) ifTrue: [
		arg1 nextPut: reasonCode.
		self encodePropertiesOn: arg1 ].! !

!MQTTPacketPubComp methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
encodePropertiesOn: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	reasonString ifNotNil: [
		temp2 addPropertyIdentifier: MQTT5Constants reasonString value: reasonString ].
	properties ifNotNil: [
		properties do: [ :p |
			p identifier = MQTT5Constants userProperty ifTrue: [ temp2 add: p ] ] ].
	temp2 encodeOn: arg1.! !

!MQTTPacketPubComp methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
decodeFrom: arg1 remainingLength: arg2
	remainingLength := arg2.
	msgID := self decode16BitIntegerFrom: arg1.
	arg2 <= 2 ifTrue: [ ^ self ].
	reasonCode := arg1 next.
	arg2 <= 3 ifTrue: [ ^ self ].
	self decodePropertiesFrom: arg1.! !

!MQTTPacketPubComp methodsFor: 'encoding' stamp: 'simba 2/16/2026 07:20'!
decodePropertiesFrom: arg1
	| temp2 |
	temp2 := MQTT5Properties new.
	temp2 decodeFrom: arg1.
	properties := temp2.
	(temp2 propertiesAt: MQTT5Constants reasonString) ifNotEmpty: [ :rs |
		reasonString := rs first value ].! !
MQTTPacket subclass: #MQTTPacketDisconnect
	instanceVariableNames: 'reasonCode properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

MQTTPacket subclass: #MQTTPacketAuth
	instanceVariableNames: 'remainingLength reasonCode properties'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MQTT5-Squeak'!

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
userProperties
	"Return user properties as Associations. Handles both encode format (Association) and decode format (Array)."
	properties ifNil: [ ^ #() ].
	^ (properties propertiesAt: MQTT5Constants userProperty)
		collect: [ :p |
			p value isArray
				ifTrue: [ p value first -> p value second ]
				ifFalse: [ p value ] ].! !

!MQTTPacketDisconnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
addUserProperty: key value: val
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: MQTT5Constants userProperty
			value: (key -> val)).! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
userProperties
	"Return user properties as Associations. Handles both encode format (Association) and decode format (Array)."
	properties ifNil: [ ^ #() ].
	^ (properties propertiesAt: MQTT5Constants userProperty)
		collect: [ :p |
			p value isArray
				ifTrue: [ p value first -> p value second ]
				ifFalse: [ p value ] ].! !

!MQTTPacketAuth methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
addUserProperty: key value: val
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addProperty:
		(MQTT5Property
			identifier: MQTT5Constants userProperty
			value: (key -> val)).! !

!MQTTTransportLayer methodsFor: 'mqtt' stamp: 'simba 2/16/2026 07:35'!
publishTopic: arg1 message: arg2 qos: arg3 retain: arg4 properties: arg5
	"Publish with MQTT5 properties. arg5 is a block that configures the packet."
	| temp5 temp6 |
	temp5 := MQTTPacketPublish new
		topic: arg1 message: arg2 asByteArray;
		retainFlag: arg4;
		qos: arg3.
	arg5 value: temp5.
	arg3 > 0 ifTrue: [
		temp5 messageID: self nextMsgID.
		temp6 := temp5 pendingJob.
		self addPendingJob: temp6 forPacketID: temp5 messageID ].
	self sendPacket: (MQTTPacketAndPendingJobPair packet: temp5 job: temp6).! !

!MQTTSocketClient methodsFor: 'mqtt' stamp: 'simba 2/16/2026 07:35'!
publishTopic: arg1 message: arg2 qos: arg3 retain: arg4 properties: arg5
	"Publish with MQTT5 properties"
	self transport
		publishTopic: arg1
		message: arg2
		qos: arg3
		retain: arg4
		properties: arg5.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'simba 2/16/2026 07:35'!
publish: aMessage onTopic: aTopic qos: qos properties: aBlock
	"Publish with MQTT5 properties. aBlock receives the MQTTPacketPublish for configuration.
	Example: client publish: data onTopic: 'test' qos: 1 properties: [:p |
		p payloadFormatIndicator: 1.
		p contentType: 'application/json'.
		p addUserProperty: 'source' value: 'squeak']"
	self socketClient
		publishTopic: aTopic
		message: aMessage
		qos: qos
		retain: false
		properties: aBlock.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'simba 2/16/2026 07:35'!
publish: aMessage onTopic: aTopic qos: qos retain: retain properties: aBlock
	"Publish with MQTT5 properties and retain flag"
	self socketClient
		publishTopic: aTopic
		message: aMessage
		qos: qos
		retain: retain
		properties: aBlock.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
addWillUserProperty: key value: val
	willProperties ifNil: [ willProperties := MQTT5Properties new ].
	willProperties addProperty:
		(MQTT5Property
			identifier: MQTT5Constants userProperty
			value: (key -> val)).! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willDelayInterval
	willProperties ifNil: [ ^ nil ].
	^ willProperties propertyAt: MQTT5Constants willDelayInterval.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willDelayInterval: anInteger
	willProperties ifNil: [ willProperties := MQTT5Properties new ].
	willProperties addProperty:
		(MQTT5Property
			identifier: MQTT5Constants willDelayInterval
			value: anInteger).! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willPayloadFormatIndicator
	willProperties ifNil: [ ^ nil ].
	^ willProperties propertyAt: MQTT5Constants payloadFormatIndicator.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willPayloadFormatIndicator: aValue
	willProperties ifNil: [ willProperties := MQTT5Properties new ].
	willProperties addProperty:
		(MQTT5Property
			identifier: MQTT5Constants payloadFormatIndicator
			value: aValue).! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willMessageExpiryInterval
	willProperties ifNil: [ ^ nil ].
	^ willProperties propertyAt: MQTT5Constants messageExpiryInterval.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willMessageExpiryInterval: anInteger
	willProperties ifNil: [ willProperties := MQTT5Properties new ].
	willProperties addProperty:
		(MQTT5Property
			identifier: MQTT5Constants messageExpiryInterval
			value: anInteger).! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willContentType
	willProperties ifNil: [ ^ nil ].
	^ willProperties propertyAt: MQTT5Constants contentType.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willContentType: aString
	willProperties ifNil: [ willProperties := MQTT5Properties new ].
	willProperties addProperty:
		(MQTT5Property
			identifier: MQTT5Constants contentType
			value: aString).! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willResponseTopic
	willProperties ifNil: [ ^ nil ].
	^ willProperties propertyAt: MQTT5Constants responseTopic.! !

!MQTTPacketConnect methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willResponseTopic: aString
	willProperties ifNil: [ willProperties := MQTT5Properties new ].
	willProperties addProperty:
		(MQTT5Property
			identifier: MQTT5Constants responseTopic
			value: aString).! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willTopic: aTopic message: aMessage retain: aBoolean qos: anInteger properties: aBlock
	"Set will with MQTT5 properties. aBlock receives the MQTTSocketClient for will property config."
	willTopic := aTopic.
	willMessage := aMessage.
	willRetainFlag := aBoolean.
	willQos := anInteger.
	aBlock value: self.! !

!MQTTSocketClient methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:35'!
willDelayInterval: anInteger
	"Set Will Delay Interval (seconds before broker publishes will)"
	| connect |
	connect := MQTTPacketConnect new.
	connect willDelayInterval: anInteger.
	willProperties := connect willProperties.! !

!MQTTClientInterface methodsFor: 'mqtt' stamp: 'simba 2/16/2026 07:35'!
willTopic: aTopic message: aMessage retain: aBoolean qos: anInteger
	"Set will message (Last Will and Testament)"
	self socketClient
		willTopic: aTopic
		message: aMessage
		retain: aBoolean
		qos: anInteger.! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:40'!
addUserProperty: key value: val
	properties ifNil: [ properties := MQTT5Properties new ].
	properties addPropertyIdentifier: MQTT5Constants userProperty value: (key -> val).! !

!MQTTPacketPublish methodsFor: 'accessing' stamp: 'simba 2/16/2026 07:40'!
userProperties
	^ (properties ifNil: [ ^ #() ]) properties
		select: [ :p | p identifier = MQTT5Constants userProperty ]
		thenCollect: [ :p | p value ].! !
