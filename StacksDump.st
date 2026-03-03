"StacksDump.st - Dump all non-system process stacks for debugging"
"Usage: Compiler evaluate: '/path/to/StacksDump.st' asFileEntry textContents"

| stream processes systemProcessNames |
stream := String new writeStream.
systemProcessNames := #(
    'Delay Scheduling'
    'User interrupt watcher'
    'Low Space Watcher'
    'Event Tickler'
    'Ephemeron finalization'
    'Idle Process'
).

processes := Processor processes select: [:p |
    | isSystemProcess |
    isSystemProcess := systemProcessNames anySatisfy: [:sysName |
        p name notNil and: [p name includesSubString: sysName]].
    isSystemProcess not and: [p isTerminated not]].

processes := processes sorted: [:a :b | a priority > b priority].

stream nextPutAll: 'Process Stack Dump - ';
    nextPutAll: DateAndTime now printString; cr; cr.

processes isEmpty
    ifTrue: [stream nextPutAll: 'No user processes found.'; cr]
    ifFalse: [
        processes do: [:process |
            | context |
            stream nextPutAll: '=== ';
                nextPutAll: (process name ifNil: ['(unnamed)']);
                nextPutAll: ' ['; nextPutAll: process statusString;
                nextPutAll: ', priority '; print: process priority;
                nextPutAll: '] ==='; cr.

            context := process isRunning
                ifTrue: [thisContext sender]
                ifFalse: [process suspendedContext].

            context ifNil: [stream nextPutAll: '  (no context)'; cr]
            ifNotNil: [
                | ctx count |
                ctx := context.
                count := 0.
                [ctx notNil] whileTrue: [
                    stream nextPutAll: '  '.
                    count < 10 ifTrue: [stream nextPutAll: ' '].
                    count < 100 ifTrue: [stream nextPutAll: ' '].
                    stream print: count; nextPutAll: ': '.
                    stream nextPutAll: ctx receiver class name;
                           nextPutAll: '>>';
                           nextPutAll: ctx selector asString; cr.
                    ctx := ctx sender.
                    count := count + 1]].
            stream cr]].

stream nextPutAll: 'Total: '; print: processes size; nextPutAll: ' process(es)'.
stream contents
