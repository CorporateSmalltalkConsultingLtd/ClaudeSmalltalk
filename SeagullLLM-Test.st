"SeagullLLMHandler Workspace Test Script
 Broker record: 'MINE' (192.168.1.158:1883, topic prefix: deepcove)
 Select all and Do It."

| handler mqtt |

"--- Start handler ---"
handler := SeagullLLMHandler
    brokerRecordNamed: 'MINE'
    imageId: 'seagull1'
    delegateEvents: nil.
handler start.
mqtt := handler mqtt.

"--- Test 1: evaluate 3 + 4 ---"
Transcript show: 'Test 1: evaluate 3 + 4'; cr.
mqtt
    publishString: (JsonObjectEncoder encode: (Dictionary new
        at: 'requestId' put: 'test-eval-001';
        at: 'action' put: 'evaluate';
        at: 'payload' put: (Dictionary new at: 'code' put: '3 + 4'; yourself);
        yourself))
    onTopic: 'claude/request/seagull1'
    qos: 1
    retain: false
    waitUntilAcknowledged: false.

"--- Test 2: browse OrderedCollection ---"
Transcript show: 'Test 2: browse OrderedCollection'; cr.
mqtt
    publishString: (JsonObjectEncoder encode: (Dictionary new
        at: 'requestId' put: 'test-browse-001';
        at: 'action' put: 'browse';
        at: 'payload' put: (Dictionary new at: 'className' put: 'OrderedCollection'; yourself);
        yourself))
    onTopic: 'claude/request/seagull1'
    qos: 1
    retain: false
    waitUntilAcknowledged: false.

"--- Test 3: methodSource OrderedCollection>>add: ---"
Transcript show: 'Test 3: methodSource OrderedCollection>>add:'; cr.
mqtt
    publishString: (JsonObjectEncoder encode: (Dictionary new
        at: 'requestId' put: 'test-source-001';
        at: 'action' put: 'methodSource';
        at: 'payload' put: (Dictionary new
            at: 'className' put: 'OrderedCollection';
            at: 'selector' put: 'add:';
            yourself);
        yourself))
    onTopic: 'claude/request/seagull1'
    qos: 1
    retain: false
    waitUntilAcknowledged: false.

"--- Test 4: listClasses prefix MQTT ---"
Transcript show: 'Test 4: listClasses prefix MQTT'; cr.
mqtt
    publishString: (JsonObjectEncoder encode: (Dictionary new
        at: 'requestId' put: 'test-list-001';
        at: 'action' put: 'listClasses';
        at: 'payload' put: (Dictionary new at: 'prefix' put: 'MQTT'; yourself);
        yourself))
    onTopic: 'claude/request/seagull1'
    qos: 1
    retain: false
    waitUntilAcknowledged: false.

"--- Test 5: hierarchy SeagullLLMHandler ---"
Transcript show: 'Test 5: hierarchy SeagullLLMHandler'; cr.
mqtt
    publishString: (JsonObjectEncoder encode: (Dictionary new
        at: 'requestId' put: 'test-hier-001';
        at: 'action' put: 'hierarchy';
        at: 'payload' put: (Dictionary new at: 'className' put: 'SeagullLLMHandler'; yourself);
        yourself))
    onTopic: 'claude/request/seagull1'
    qos: 1
    retain: false
    waitUntilAcknowledged: false.

"--- Test 6: subclasses Collection ---"
Transcript show: 'Test 6: subclasses Collection'; cr.
mqtt
    publishString: (JsonObjectEncoder encode: (Dictionary new
        at: 'requestId' put: 'test-subs-001';
        at: 'action' put: 'subclasses';
        at: 'payload' put: (Dictionary new at: 'className' put: 'Collection'; yourself);
        yourself))
    onTopic: 'claude/request/seagull1'
    qos: 1
    retain: false
    waitUntilAcknowledged: false.

"--- Test 7: error handling — bad class ---"
Transcript show: 'Test 7: error handling - bad class'; cr.
mqtt
    publishString: (JsonObjectEncoder encode: (Dictionary new
        at: 'requestId' put: 'test-error-001';
        at: 'action' put: 'browse';
        at: 'payload' put: (Dictionary new at: 'className' put: 'NoSuchClassXYZ'; yourself);
        yourself))
    onTopic: 'claude/request/seagull1'
    qos: 1
    retain: false
    waitUntilAcknowledged: false.

"--- Test 8: unknown action ---"
Transcript show: 'Test 8: unknown action'; cr.
mqtt
    publishString: (JsonObjectEncoder encode: (Dictionary new
        at: 'requestId' put: 'test-unknown-001';
        at: 'action' put: 'bogusAction';
        at: 'payload' put: Dictionary new;
        yourself))
    onTopic: 'claude/request/seagull1'
    qos: 1
    retain: false
    waitUntilAcknowledged: false.

"--- Done ---"
Transcript show: 'All 8 tests sent. Check MQTT responses.'; cr.

"--- To stop later: handler stop ---"
