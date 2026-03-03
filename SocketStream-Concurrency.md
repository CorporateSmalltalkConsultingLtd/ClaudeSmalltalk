# SocketStream Concurrency in Squeak Smalltalk

## Smalltalk Process Model

Smalltalk uses **cooperative multitasking** (green threads) within a single OS thread. This is fundamentally different from preemptive multitasking - process switches only occur at well-defined yield points.

### When Process Switches Occur

| Trigger | Example |
|---------|---------|
| Explicit yield | `Processor yield` |
| Semaphore wait | `semaphore wait`, `waitTimeoutMSecs:` |
| Delay wait | `(Delay forMilliseconds: n) wait` |
| Priority preemption | Higher priority process becomes runnable |

**Key insight**: Code between yield points executes atomically from the scheduler's perspective.

## SocketStream Architecture

### Instance Variables

```smalltalk
SocketStream
    socket           "The underlying Socket"
    inBuffer         "ByteArray for incoming data"
    outBuffer        "ByteArray for outgoing data"
    inNextToWrite    "Position in inBuffer"
    outNextToWrite   "Position in outBuffer"
    lastRead         "Last read position"
    autoFlush        "Boolean - flush on every write?"
    bufferSize       "Size threshold for auto-flush"
```

### No Internal Synchronization

SocketStream has **no mutex or lock** on buffer access. It assumes single-threaded access per direction.

## Yield Points in SocketStream

### Writing Path

```
nextPutAll: → checkFlush → flush → sendData:count: → sendSomeData: → waitForSendDoneFor:
                                        ↓                                    ↓
                                 Processor yield              writeSemaphore waitTimeoutMSecs:
                                 (when buffer full)                    (YIELD POINT)
```

### Reading Path

```
next/nextLine → receiveData → waitForData → waitForDataFor:ifClosed:ifTimedOut:
                                                          ↓
                                            readSemaphore waitTimeoutMSecs:
                                                    (YIELD POINT)
```

### Method Analysis

**nextPutAll: (writing)**
```smalltalk
nextPutAll: aCollection
    toPut := binary ifTrue: [aCollection asByteArray]
                    ifFalse: [aCollection asString].  "← no yield"
    self adjustOutBuffer: toPut size.                 "← no yield"
    outBuffer replaceFrom: outNextToWrite
              to: outNextToWrite + toPut size - 1
              with: toPut startingAt: 1.              "← no yield"
    outNextToWrite := outNextToWrite + toPut size.   "← no yield"
    self checkFlush.                                  "← YIELD if autoFlush & buffer full"
```

- **autoFlush = false**: Entire operation is atomic
- **autoFlush = true**: Can yield during flush

**flush (sending)**
```smalltalk
flush
    (outNextToWrite > 1 and: [socket isOtherEndClosed not]) ifTrue: [
        self sendData: outBuffer count: outNextToWrite - 1.  "← YIELD POINT"
        outNextToWrite := 1]
```

**sendData:count: (socket level)**
```smalltalk
sendData: buffer count: n
    [totalSent < n] whileTrue: [
        sent := self sendSomeData: buffer startIndex: totalSent+1 count: n-totalSent.
        totalSent := totalSent + sent.
        sent = 0 ifTrue: [Processor yield]]  "← YIELD POINT"
```

## Race Condition Scenarios

### When Races CAN Occur

| Scenario | Condition | Risk |
|----------|-----------|------|
| Multiple writers | autoFlush = true | Buffer corruption during flush yield |
| Multiple writers | Explicit flush calls | Interleaved data |
| Multiple readers | Buffer empty | Corruption when waiting for data |

### When Races CANNOT Occur

| Scenario | Why Safe |
|----------|----------|
| Single writer, single reader | Separate buffers, no conflict |
| Multiple writers, autoFlush = false, no flush | No yield points reached |
| All writes complete before flush | Atomic buffer operations |

## Mitigation Strategies

### 1. One Process Per Direction (Recommended)

```smalltalk
"Dedicated writer process"
writerProcess := [
    [queue nextGet ifNotNil: [:msg |
        stream nextPutAll: msg; flush]] repeat
] forkAt: Processor userSchedulingPriority named: 'Socket Writer'.

"Dedicated reader process"
readerProcess := [
    [handler handleMessage: stream nextLine] repeat
] forkAt: Processor userSchedulingPriority named: 'Socket Reader'.
```

### 2. Disable autoFlush, Control Flush Points

```smalltalk
stream autoFlush: false.

"Batch writes atomically, single flush"
stream nextPutAll: header.
stream nextPutAll: body.
stream nextPutAll: trailer.
stream flush.  "Only yield point - atomic up to here"
```

### 3. SharedQueue for Multi-Producer

```smalltalk
outQueue := SharedQueue new.

"Any process can safely enqueue"
outQueue nextPut: 'message from process A'.
outQueue nextPut: 'message from process B'.

"Single writer drains queue"
writerProcess := [
    [stream nextPutAll: outQueue next; flush] repeat
] fork.
```

### 4. Mutex Only Around Yield Points

```smalltalk
flushMutex := Semaphore forMutualExclusion.

writeAndFlush: data
    "Mutex only around the flush (the yield point)"
    stream nextPutAll: data.  "Atomic, no yield"
    flushMutex critical: [stream flush]  "Protect the yield"
```

### 5. Higher Priority for Socket Process

```smalltalk
"Socket handler at higher priority won't be preempted by lower priority user code"
socketProcess := [self runSocketLoop]
    forkAt: Processor userSchedulingPriority + 1
    named: 'Socket Handler'.
```

## Summary Table

| Risk | Mitigation |
|------|------------|
| Multiple writers | SharedQueue + single writer process |
| Multiple readers | Single reader process + dispatch to handlers |
| autoFlush races | `autoFlush: false`, explicit controlled flush |
| Priority preemption | Run socket code at higher priority |
| General safety | One process per direction (reader + writer) |

## Best Practice Pattern

```smalltalk
Object subclass: #SafeSocketHandler
    instanceVariableNames: 'socket stream outQueue readerProcess writerProcess'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Network-Safe'

SafeSocketHandler>>initialize
    outQueue := SharedQueue new.

SafeSocketHandler>>connectTo: host port: port
    socket := Socket newTCP.
    socket connectTo: (NetNameResolver addressForName: host) port: port.
    stream := SocketStream on: socket.
    stream autoFlush: false.
    self startProcesses.

SafeSocketHandler>>startProcesses
    readerProcess := [self readLoop]
        forkAt: Processor userSchedulingPriority + 1
        named: 'Socket Reader'.
    writerProcess := [self writeLoop]
        forkAt: Processor userSchedulingPriority + 1
        named: 'Socket Writer'.

SafeSocketHandler>>readLoop
    [[self handleIncoming: stream nextLine] repeat]
        on: ConnectionClosed
        do: [:ex | self connectionLost].

SafeSocketHandler>>writeLoop
    [[stream nextPutAll: outQueue next; flush] repeat]
        on: ConnectionClosed
        do: [:ex | self connectionLost].

SafeSocketHandler>>send: aString
    "Safe to call from any process"
    outQueue nextPut: aString.
```

## References

- `SocketStream` class comment in Squeak
- `Socket` class for low-level semaphore handling
- `SharedQueue` for thread-safe producer/consumer pattern
- `Semaphore forMutualExclusion` for critical sections
