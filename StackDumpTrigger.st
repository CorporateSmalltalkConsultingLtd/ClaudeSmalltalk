| triggerFile outputFile watchdogProcess systemProcessNames |

triggerFile := '/tmp/dump-stacks-trigger'.
outputFile := '/tmp/stack-dump.txt'.

systemProcessNames := #(
    'Delay Scheduling'
    'User interrupt watcher'
    'Low Space Watcher'
    'Event Tickler'
    'Ephemeron finalization'
    'Idle Process'
    'Stack Dump Watcher'
).

(Processor processes anySatisfy: [:p |
    p name notNil and: [p name = 'Stack Dump Watcher']])
ifTrue: [^ 'Stack Dump Watcher already running'].

watchdogProcess := [
    [true] whileTrue: [
        [
            triggerFile asFileEntry exists ifTrue: [
                | stream processes |
                triggerFile asFileEntry delete.

                processes := Processor processes select: [:p |
                    | isSystem |
                    isSystem := systemProcessNames anySatisfy: [:sysName |
                        p name notNil and: [p name includesSubString: sysName]].
                    isSystem not and: [p isTerminated not]].
                processes := processes sorted: [:a :b | a priority > b priority].

                stream := String new writeStream.
                stream nextPutAll: 'Stack Dump - ';
                    nextPutAll: DateAndTime now printString; cr; cr.

                processes do: [:process |
                    | context |
                    stream nextPutAll: '=== ';
                        nextPutAll: (process name ifNil: ['(unnamed)']);
                        nextPutAll: ' ['; nextPutAll: process statusString;
                        nextPutAll: ', priority '; print: process priority;
                        nextPutAll: '] ==='; cr.

                    context := process isRunning
                        ifTrue: [thisContext sender sender sender sender]
                        ifFalse: [process suspendedContext].

                    context ifNil: [stream nextPutAll: '  (no context)'; cr]
                    ifNotNil: [
                        | ctx count |
                        ctx := context.
                        count := 0.
                        [ctx notNil and: [count < 50]] whileTrue: [
                            stream nextPutAll: '  '.
                            count < 10 ifTrue: [stream nextPutAll: ' '].
                            stream print: count; nextPutAll: ': '.
                            stream nextPutAll: ctx receiver class name;
                                nextPutAll: '>>';
                                nextPutAll: ctx selector asString; cr.
                            ctx := ctx sender.
                            count := count + 1].
                        ctx ifNotNil: [stream nextPutAll: '  ... (more frames)'; cr]].
                    stream cr].

                stream nextPutAll: 'Total: '; print: processes size; nextPutAll: ' process(es)'.

                outputFile asFileEntry forceWriteStreamDo: [:s |
                    s nextPutAll: stream contents]]
        ] on: Error do: [:e |
            outputFile asFileEntry forceWriteStreamDo: [:s |
                s nextPutAll: 'Watchdog error: '; nextPutAll: e messageText]].

        (Delay forMilliseconds: 500) wait]
] forkAt: Processor highIOPriority named: 'Stack Dump Watcher'.

true
