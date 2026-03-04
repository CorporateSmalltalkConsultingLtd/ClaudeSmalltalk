'From Cuis6.3 [latest update: #6156] on 2 January 2024 at 9:45:41 am'!
'Description OSProcess provides access to operating system functions, including pipes, child process creation, and control of the Squeak VM process.'!
!provides: 'OSProcess' 1 36!
!requires: 'Network-Kernel' 1 1 nil!
!requires: 'SqueakCompatibility' 1 4 nil!
SystemOrganization addCategory: #'OSProcess-Base'!
SystemOrganization addCategory: #'OSProcess-Mac'!
SystemOrganization addCategory: #'OSProcess-OS2'!
SystemOrganization addCategory: #'OSProcess-RiscOS'!
SystemOrganization addCategory: #'OSProcess-Unix'!
SystemOrganization addCategory: #'OSProcess-Win32'!
SystemOrganization addCategory: #'OSProcess-AIO'!
SystemOrganization addCategory: #'OSProcess-Tests'!


!classDefinition: #OSProcessAccessor category: #'OSProcess-Base'!
Model subclass: #OSProcessAccessor
	instanceVariableNames: 'sessionIdentifier grimReaper canObtainSessionIdentifierFromPlugin'
	classVariableNames: 'EmulateWin32FileLocking FileLockRegistry ThisOSProcessAccessor'
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'OSProcessAccessor class' category: #'OSProcess-Base'!
OSProcessAccessor class
	instanceVariableNames: ''!

!classDefinition: #MacOSProcessAccessor category: #'OSProcess-Mac'!
OSProcessAccessor subclass: #MacOSProcessAccessor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Mac'!
!classDefinition: 'MacOSProcessAccessor class' category: #'OSProcess-Mac'!
MacOSProcessAccessor class
	instanceVariableNames: ''!

!classDefinition: #OS2OSProcessAccessor category: #'OSProcess-OS2'!
OSProcessAccessor subclass: #OS2OSProcessAccessor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-OS2'!
!classDefinition: 'OS2OSProcessAccessor class' category: #'OSProcess-OS2'!
OS2OSProcessAccessor class
	instanceVariableNames: ''!

!classDefinition: #RiscOSProcessAccessor category: #'OSProcess-RiscOS'!
OSProcessAccessor subclass: #RiscOSProcessAccessor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-RiscOS'!
!classDefinition: 'RiscOSProcessAccessor class' category: #'OSProcess-RiscOS'!
RiscOSProcessAccessor class
	instanceVariableNames: ''!

!classDefinition: #UnixOSProcessAccessor category: #'OSProcess-Unix'!
OSProcessAccessor subclass: #UnixOSProcessAccessor
	instanceVariableNames: 'sigChldSemaphore'
	classVariableNames: 'ThisProcessPid'
	poolDictionaries: ''
	category: 'OSProcess-Unix'!
!classDefinition: 'UnixOSProcessAccessor class' category: #'OSProcess-Unix'!
UnixOSProcessAccessor class
	instanceVariableNames: ''!

!classDefinition: #WindowsOSProcessAccessor category: #'OSProcess-Win32'!
OSProcessAccessor subclass: #WindowsOSProcessAccessor
	instanceVariableNames: 'sigChldSemaphore semaIndex childWatcherThread'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Win32'!
!classDefinition: 'WindowsOSProcessAccessor class' category: #'OSProcess-Win32'!
WindowsOSProcessAccessor class
	instanceVariableNames: ''!

!classDefinition: #AioEventHandler category: #'OSProcess-AIO'!
Model subclass: #AioEventHandler
	instanceVariableNames: 'semaphore semaIndex handlerProc descriptor'
	classVariableNames: 'AioPluginPresent'
	poolDictionaries: ''
	category: 'OSProcess-AIO'!
!classDefinition: 'AioEventHandler class' category: #'OSProcess-AIO'!
AioEventHandler class
	instanceVariableNames: ''!

!classDefinition: #PseudoAioEventHandler category: #'OSProcess-AIO'!
Model subclass: #PseudoAioEventHandler
	instanceVariableNames: 'eventGenerator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-AIO'!
!classDefinition: 'PseudoAioEventHandler class' category: #'OSProcess-AIO'!
PseudoAioEventHandler class
	instanceVariableNames: ''!

!classDefinition: #AttachableFileStream category: #'OSProcess-Base'!
StandardFileStream subclass: #AttachableFileStream
	instanceVariableNames: 'autoClose'
	classVariableNames: 'UseIOHandle'
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'AttachableFileStream class' category: #'OSProcess-Base'!
AttachableFileStream class
	instanceVariableNames: ''!

!classDefinition: #AsyncFileReadStream category: #'OSProcess-Base'!
AttachableFileStream subclass: #AsyncFileReadStream
	instanceVariableNames: 'eventHandler'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'AsyncFileReadStream class' category: #'OSProcess-Base'!
AsyncFileReadStream class
	instanceVariableNames: ''!

!classDefinition: #BufferedAsyncFileReadStream category: #'OSProcess-Base'!
AsyncFileReadStream subclass: #BufferedAsyncFileReadStream
	instanceVariableNames: 'nonBlockingMode readBuffer readSyncSemaphore dataAvailableSemaphore'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'BufferedAsyncFileReadStream class' category: #'OSProcess-Base'!
BufferedAsyncFileReadStream class
	instanceVariableNames: ''!

!classDefinition: #ResettableBufferStream category: #'OSProcess-Base'!
ReadWriteStream subclass: #ResettableBufferStream
	instanceVariableNames: 'resetLimit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'ResettableBufferStream class' category: #'OSProcess-Base'!
ResettableBufferStream class
	instanceVariableNames: ''!

!classDefinition: #ExternalPipe category: #'OSProcess-Base'!
Stream subclass: #ExternalPipe
	instanceVariableNames: 'writer reader blocking'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'ExternalPipe class' category: #'OSProcess-Base'!
ExternalPipe class
	instanceVariableNames: ''!

!classDefinition: #OSPipe category: #'OSProcess-Base'!
ExternalPipe subclass: #OSPipe
	instanceVariableNames: 'nextChar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'OSPipe class' category: #'OSProcess-Base'!
OSPipe class
	instanceVariableNames: ''!

!classDefinition: #AbstractUnixProcessFileLockingTestCase category: #'OSProcess-Tests'!
TestCase subclass: #AbstractUnixProcessFileLockingTestCase
	instanceVariableNames: 'accessor fileStream delay remoteProcess initialCompatibilitySetting'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Tests'!
!classDefinition: 'AbstractUnixProcessFileLockingTestCase class' category: #'OSProcess-Tests'!
AbstractUnixProcessFileLockingTestCase class
	instanceVariableNames: ''!

!classDefinition: #UnixProcessUnixFileLockingTestCase category: #'OSProcess-Tests'!
AbstractUnixProcessFileLockingTestCase subclass: #UnixProcessUnixFileLockingTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Tests'!
!classDefinition: 'UnixProcessUnixFileLockingTestCase class' category: #'OSProcess-Tests'!
UnixProcessUnixFileLockingTestCase class
	instanceVariableNames: ''!

!classDefinition: #UnixProcessWin32FileLockingTestCase category: #'OSProcess-Tests'!
AbstractUnixProcessFileLockingTestCase subclass: #UnixProcessWin32FileLockingTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Tests'!
!classDefinition: 'UnixProcessWin32FileLockingTestCase class' category: #'OSProcess-Tests'!
UnixProcessWin32FileLockingTestCase class
	instanceVariableNames: ''!

!classDefinition: #AioEventHandlerTestCase category: #'OSProcess-Tests'!
TestCase subclass: #AioEventHandlerTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Tests'!
!classDefinition: 'AioEventHandlerTestCase class' category: #'OSProcess-Tests'!
AioEventHandlerTestCase class
	instanceVariableNames: ''!

!classDefinition: #OSPipeTestCase category: #'OSProcess-Tests'!
TestCase subclass: #OSPipeTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Tests'!
!classDefinition: 'OSPipeTestCase class' category: #'OSProcess-Tests'!
OSPipeTestCase class
	instanceVariableNames: ''!

!classDefinition: #ResettableBufferStreamTest category: #'OSProcess-Tests'!
TestCase subclass: #ResettableBufferStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Tests'!
!classDefinition: 'ResettableBufferStreamTest class' category: #'OSProcess-Tests'!
ResettableBufferStreamTest class
	instanceVariableNames: ''!

!classDefinition: #UnixProcessAccessorTestCase category: #'OSProcess-Tests'!
TestCase subclass: #UnixProcessAccessorTestCase
	instanceVariableNames: 'accessor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Tests'!
!classDefinition: 'UnixProcessAccessorTestCase class' category: #'OSProcess-Tests'!
UnixProcessAccessorTestCase class
	instanceVariableNames: ''!

!classDefinition: #UnixProcessFileLockTestCase category: #'OSProcess-Tests'!
TestCase subclass: #UnixProcessFileLockTestCase
	instanceVariableNames: 'fileStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Tests'!
!classDefinition: 'UnixProcessFileLockTestCase class' category: #'OSProcess-Tests'!
UnixProcessFileLockTestCase class
	instanceVariableNames: ''!

!classDefinition: #UnixProcessTestCase category: #'OSProcess-Tests'!
TestCase subclass: #UnixProcessTestCase
	instanceVariableNames: 'thisOSProcess'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Tests'!
!classDefinition: 'UnixProcessTestCase class' category: #'OSProcess-Tests'!
UnixProcessTestCase class
	instanceVariableNames: ''!

!classDefinition: #OSFileLock category: #'OSProcess-Base'!
Object subclass: #OSFileLock
	instanceVariableNames: 'fileStream exclusive'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'OSFileLock class' category: #'OSProcess-Base'!
OSFileLock class
	instanceVariableNames: ''!

!classDefinition: #OSFileRegionLock category: #'OSProcess-Base'!
OSFileLock subclass: #OSFileRegionLock
	instanceVariableNames: 'interval'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'OSFileRegionLock class' category: #'OSProcess-Base'!
OSFileRegionLock class
	instanceVariableNames: ''!

!classDefinition: #OSProcess category: #'OSProcess-Base'!
Object subclass: #OSProcess
	instanceVariableNames: 'pid'
	classVariableNames: 'UseIOHandle'
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'OSProcess class' category: #'OSProcess-Base'!
OSProcess class
	instanceVariableNames: ''!

!classDefinition: #ExternalOSProcess category: #'OSProcess-Base'!
OSProcess subclass: #ExternalOSProcess
	instanceVariableNames: 'runState initialStdIn initialStdOut initialStdErr'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'ExternalOSProcess class' category: #'OSProcess-Base'!
ExternalOSProcess class
	instanceVariableNames: ''!

!classDefinition: #ExternalMacOSProcess category: #'OSProcess-Mac'!
ExternalOSProcess subclass: #ExternalMacOSProcess
	instanceVariableNames: 'ppid exitStatus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Mac'!
!classDefinition: 'ExternalMacOSProcess class' category: #'OSProcess-Mac'!
ExternalMacOSProcess class
	instanceVariableNames: ''!

!classDefinition: #ExternalOS2Process category: #'OSProcess-OS2'!
ExternalOSProcess subclass: #ExternalOS2Process
	instanceVariableNames: 'ppid exitStatus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-OS2'!
!classDefinition: 'ExternalOS2Process class' category: #'OSProcess-OS2'!
ExternalOS2Process class
	instanceVariableNames: ''!

!classDefinition: #ExternalRiscOSProcess category: #'OSProcess-RiscOS'!
ExternalOSProcess subclass: #ExternalRiscOSProcess
	instanceVariableNames: 'ppid exitStatus'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-RiscOS'!
!classDefinition: 'ExternalRiscOSProcess class' category: #'OSProcess-RiscOS'!
ExternalRiscOSProcess class
	instanceVariableNames: ''!

!classDefinition: #ExternalUnixOSProcess category: #'OSProcess-Unix'!
ExternalOSProcess subclass: #ExternalUnixOSProcess
	instanceVariableNames: 'ppid pwd exitStatus programName arguments initialEnvironment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Unix'!
!classDefinition: 'ExternalUnixOSProcess class' category: #'OSProcess-Unix'!
ExternalUnixOSProcess class
	instanceVariableNames: ''!

!classDefinition: #ExternalWindowsOSProcess category: #'OSProcess-Win32'!
ExternalOSProcess subclass: #ExternalWindowsOSProcess
	instanceVariableNames: 'ppid exitStatus handle threads commandLine pwd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Win32'!
!classDefinition: 'ExternalWindowsOSProcess class' category: #'OSProcess-Win32'!
ExternalWindowsOSProcess class
	instanceVariableNames: ''!

!classDefinition: #ThisOSProcess category: #'OSProcess-Base'!
OSProcess subclass: #ThisOSProcess
	instanceVariableNames: 'sessionID stdIn stdOut stdErr processAccessor childProcessList accessProtect'
	classVariableNames: 'ChildListSize ThisInstance'
	poolDictionaries: ''
	category: 'OSProcess-Base'!
!classDefinition: 'ThisOSProcess class' category: #'OSProcess-Base'!
ThisOSProcess class
	instanceVariableNames: ''!

!classDefinition: #MacProcess category: #'OSProcess-Mac'!
ThisOSProcess subclass: #MacProcess
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Mac'!
!classDefinition: 'MacProcess class' category: #'OSProcess-Mac'!
MacProcess class
	instanceVariableNames: ''!

!classDefinition: #OS2Process category: #'OSProcess-OS2'!
ThisOSProcess subclass: #OS2Process
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-OS2'!
!classDefinition: 'OS2Process class' category: #'OSProcess-OS2'!
OS2Process class
	instanceVariableNames: ''!

!classDefinition: #RiscOSProcess category: #'OSProcess-RiscOS'!
ThisOSProcess subclass: #RiscOSProcess
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-RiscOS'!
!classDefinition: 'RiscOSProcess class' category: #'OSProcess-RiscOS'!
RiscOSProcess class
	instanceVariableNames: ''!

!classDefinition: #UnixProcess category: #'OSProcess-Unix'!
ThisOSProcess subclass: #UnixProcess
	instanceVariableNames: 'ppid pthread path programName arguments environment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Unix'!
!classDefinition: 'UnixProcess class' category: #'OSProcess-Unix'!
UnixProcess class
	instanceVariableNames: ''!

!classDefinition: #WindowsProcess category: #'OSProcess-Win32'!
ThisOSProcess subclass: #WindowsProcess
	instanceVariableNames: 'processHandle environment mainThread threads'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Win32'!
!classDefinition: 'WindowsProcess class' category: #'OSProcess-Win32'!
WindowsProcess class
	instanceVariableNames: ''!

!classDefinition: #UnixProcessExitStatus category: #'OSProcess-Unix'!
Object subclass: #UnixProcessExitStatus
	instanceVariableNames: 'intValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Unix'!
!classDefinition: 'UnixProcessExitStatus class' category: #'OSProcess-Unix'!
UnixProcessExitStatus class
	instanceVariableNames: ''!

!classDefinition: #WindowsThread category: #'OSProcess-Win32'!
Object subclass: #WindowsThread
	instanceVariableNames: 'threadID handle runState'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-Win32'!
!classDefinition: 'WindowsThread class' category: #'OSProcess-Win32'!
WindowsThread class
	instanceVariableNames: ''!

!classDefinition: #AioEventHandlerExample category: #'OSProcess-AIO'!
Object subclass: #AioEventHandlerExample
	instanceVariableNames: 'handler ioStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OSProcess-AIO'!
!classDefinition: 'AioEventHandlerExample class' category: #'OSProcess-AIO'!
AioEventHandlerExample class
	instanceVariableNames: ''!


!OSProcessAccessor commentStamp: 'dtl 7/7/2010 07:58' prior: 0!
I am an abstract class whose subclasses provide access to an operating system process, such as the process in which the Squeak VM is currently running. My subclasses collaborate with instances of OSProcess subclasses.

The single instance ThisOSProcessAccessor provides access to the OS process in which the virtual machine is running. On Unix, this instance sets a signal handler to respond to externally generated sigchld signals. This must be done after each image restart in order to call a primitive which informs the VM of the  identity of the semaphore to signal. A similar mechanism is used for Windows to obtain exit status of external OS processes. ThisOSProcessor maintains a process that waits on child exit events and updates a dependent OSProcess of changes to external OS processes.

When an image is restarted on a different kind of platform, a new instance is assigned to ThisOSProcessAccessor to provide access to the virtual machine OS process.
!

!MacOSProcessAccessor commentStamp: '<historical>' prior: 0!
I provide access to an operating system process, such as the process in which the Squeak VM is currently running. I am based on the Macintosh process model.!

!OS2OSProcessAccessor commentStamp: '<historical>' prior: 0!
I provide access to an operating system process, such as the process in which the Squeak VM is currently running. I am based on the OS2 process model.!

!RiscOSProcessAccessor commentStamp: '<historical>' prior: 0!
I provide access to the operating system process in which the Squeak VM is currently running. I am based on the RiscOS task model. There is only one instance of me, and instances of RiscOSProcess depend on me to provide access to the operating system process which they represent. I know how to create child processes. I use a semaphore to receive signals when child processes die, and I notify my dependents (instances ofRiscOSProcess) when these events occur.
!

!UnixOSProcessAccessor commentStamp: '<historical>' prior: 0!
I provide access to the operating system process in which the Squeak VM is currently running. I am based on the Unix process model. There is only one instance of me, and instances of UnixOSProcess depend on me to provide access to the operating system process which they represent.

I know how to create child processes. I use a semaphore to receive signals when child processes die, and I notify my dependents (instances of UnixOSProcess) when these events occur.

!

!WindowsOSProcessAccessor commentStamp: '<historical>' prior: 0!
I provide access to an operating system process, such as the process in which the Squeak VM is currently running. I am based on the Win32 process model for Windows and Windows NT.!

!AioEventHandler commentStamp: 'dtl 11/25/2006 15:55' prior: 0!
AioEventHandler responds to external IO events, such as data available on a file descriptor. When an external IO event is received, an instance of AioEventHandler sends #changed to itself to notify its dependents that the event has occurred.!

!PseudoAioEventHandler commentStamp: 'dtl 11/25/2006 10:42' prior: 0!
PseudoAioEventHandler is a replacement for AioEventHandler for use when an AioPlugin is not present. It creates a polling loop by generating #changed: events periodically. With a real AioEventHandler, events are generated only when actual IO activity occurs, while the PseudoAioEventHandler produces regularly timed events regardless of whether any actual IO changes have happened.!

!AttachableFileStream commentStamp: '<historical>' prior: 0!
I am a stream on an input or output channel provided by the underlying operating system. I behave like an ordinary file stream, except that I can attach myself to an input or output stream which has already been opened by the underlying operating system.!

!AsyncFileReadStream commentStamp: 'dtl 7/9/2003 21:04' prior: 0!
AsyncFileReadStream implements event-driven read behavior on a file stream. Whenever data is available, a #changed event is generated. An AsyncFileReadStream expects to have a client object respond immediately to the change notification by reading the available data, otherwise a possibly endless stream of change notifications will be generated.

AsyncFileReadStream requires aio support in the AioPlugin module.!

!BufferedAsyncFileReadStream commentStamp: 'dtl 3/7/2006 06:55' prior: 0!
BufferedAsyncFileReadStream adds output buffering behavior to an event driven file stream, permitting blocking reads without risk of blocking the Squeak VM. This is useful for OS pipes, for which Squeak may wish to read and write the pipe without concern for VM deadlocks.

A BufferedAsyncFileReadStream may be set for either blocking or nonblocking reads. When in blocking mode, a Smalltalk Process that requests a read will be blocked until data is available, but the VM will not be blocked and other Smalltalk Processes can proceed normally.

Whenever data becomes available, a dataAvailableSemaphore is signalled and a #changed event is generated.!

!ResettableBufferStream commentStamp: 'dtl 7/30/2023 21:16' prior: 0!
A ResettableBufferStream can trim all previously read elements to save space in its collection. This allows a writer to add continuously to the stream as long as the stream collection is periodically trimmed by a readerthat calls pruneCollection after reading.
!

!ExternalPipe commentStamp: 'dtl 3/10/2006 11:06' prior: 0!
I represent a pipe provided by the underlying operating system, such as a Unix pipe. I have a reader stream and a writer stream which behave similarly to a read-only FileStream and a writeable FileStream.

Subclasses implement buffering behavior for the reader end of a pipe.!

!OSPipe commentStamp: 'dtl 3/8/2006 07:27' prior: 0!
I represent a pipe provided by the underlying operating system, such as a Unix pipe. I have a reader stream and a writer stream which behave similarly to a read-only FileStream and a writeable FileStream.

I use a single-character buffer to implement #peek without losing data from the external OS pipe.!

!AbstractUnixProcessFileLockingTestCase commentStamp: 'dtl 4/30/2006 14:02' prior: 0!
Test file locking with the UnixOSProcessPlugin. The test suite requires that OSProcess and CommandShell be loaded in the image.

These tests rely on a remote Squeak image to test file locks between cooperating Unix processes. This may be timing dependent (see #delay, set in #setUp, and cleanup in #tearDown). In case of intermittent failures, try running the failed test individually. In some cases it may be necessary to restart Squeak in order to clear leftover file locks from previous failed tests.!

!UnixProcessUnixFileLockingTestCase commentStamp: 'dtl 4/30/2006 14:03' prior: 0!
Test file locking with the UnixOSProcessPlugin using Unix file locking semantics. The test suite requires that OSProcess and CommandShell be loaded in the image.

These tests rely on a remote Squeak image to test file locks between cooperating Unix processes. This may be timing dependent (see #delay, set in #setUp, and cleanup in #tearDown). In case of intermittent failures, try running the failed test individually. In some cases it may be necessary to restart Squeak in order to clear leftover file locks from previous failed tests.!

!UnixProcessWin32FileLockingTestCase commentStamp: 'dtl 4/30/2006 14:03' prior: 0!
Test file locking with the UnixOSProcessPlugin using Windows file locking semantics. The test suite requires that OSProcess and CommandShell be loaded in the image.

These tests rely on a remote Squeak image to test file locks between cooperating Unix processes. This may be timing dependent (see #delay, set in #setUp, and cleanup in #tearDown). In case of intermittent failures, try running the failed test individually. In some cases it may be necessary to restart Squeak in order to clear leftover file locks from previous failed tests.!

!AioEventHandlerTestCase commentStamp: 'dtl 7/10/2005 15:41' prior: 0!
Test AioEventHandler and AioPlugin. Provides fair coverage of IO readable events, minimal coverage of IO writable events, and no real coverage for IO exception events. The writable events and exception events probably work, but this test suite will not prove it.!

!OSPipeTestCase commentStamp: '<historical>' prior: 0!
Test operation of OSPipe in blocking and nonBlocking mode.!

!UnixProcessAccessorTestCase commentStamp: '<historical>' prior: 0!
Unit tests for the UnixProcessAccessor.!

!UnixProcessFileLockTestCase commentStamp: 'dtl 3/7/2005 21:57' prior: 0!
This test case was provided by Julian Fitzell. It provides more file locking tests in addition to those in UnitProcessFileLockingTestCase.!

!UnixProcessTestCase commentStamp: '<historical>' prior: 0!
Unit tests for the Unix portion of OSProcess.!

!OSFileLock commentStamp: 'dtl 2/23/2004 19:36' prior: 0!
I describe the region representing the entire addressable space of an external file, including regions that have not yet been allocated for use by the file. On platforms that support file locking, an OSFileLock is used to describe a lock on the entire file.
!

!OSFileRegionLock commentStamp: 'jf 2/22/2004 19:50' prior: 0!
I describe an addressable region of contiguous bytes in an external file. On platforms that support file locking, an OSFileRegionLock is used to specify a portion of the file to be locked.
!

!OSProcess commentStamp: '<historical>' prior: 0!
I represent an operating system process, such as the process in which the Squeak VM is currently running. My subclasses implement system specific features for Unix, Windows, MacOS, or other operating systems.
!

!ExternalOSProcess commentStamp: '<historical>' prior: 0!
I represent an OSProcess other than the process in which this Squeak is executing. I maintain information about the state of the external process during and after the lifetime of the process.!

!ExternalMacOSProcess commentStamp: '<historical>' prior: 0!
I represent an external MacOS process other than the process in which this Squeak is executing. I maintain information about the state of the external process during and after the lifetime of the process. In particular, I hold the exit status of the process after it completes execution. When the external process changes state (e.g. it exits), the VM signals a Squeak semaphore. A singleton MacOSProcessAccessor maintains a process which waits on the semaphore, and sends a changed: #childProcessStatus message to itself, thereby notifying its dependent MacOSProcess (a singleton) to check the status of all its ExternalMacOSProcess children, and #update: them accordingly.!

!ExternalOS2Process commentStamp: '<historical>' prior: 0!
I represent an external OS2 process other than the process in which this Squeak is executing. I maintain information about the state of the external process during and after the lifetime of the process. In particular, I hold the exit status of the process after it completes execution. When the external process changes state (e.g. it exits), the VM signals a Squeak semaphore. A singleton OS2ProcessAccessor maintains a process which waits on the semaphore, and sends a changed: #childProcessStatus message to itself, thereby notifying its dependent OS2Process (a singleton) to check the status of all its ExternalOS2Process children, and #update: them accordingly.!

!ExternalRiscOSProcess commentStamp: '<historical>' prior: 0!
I represent an external RiscOS task other than the process in which this Squeak is executing. I maintain information about the state of the external task during and after the lifetime of the task. In particular, I hold the exit status of the task after it completes execution. When the external task changes state (e.g. it exits), the VM signals a Squeak semaphore. A singleton RiscOSProcessAccessor maintains a process which waits on the semaphore, and sends a changed: #childProcessStatus message to itself, thereby notifying its dependent RiscOSProcess (a singleton) to check the status of all its ExternalRiscOSProcess children, and #update: them accordingly.
!

!ExternalUnixOSProcess commentStamp: '<historical>' prior: 0!
I represent an external Unix process other than the process in which this Squeak is executing. I maintain information about the state of the external process during and after the lifetime of the process. In particular, I hold the exit status of the process after it completes execution. When the external process changes state (e.g. it exits), the VM signals a Squeak semaphore. A singleton UnixProcessAccessor maintains a process which waits on the semaphore, and sends a changed: #childProcessStatus message to itself, thereby notifying its dependent UnixProcess (a singleton) to check the status of all its ExternalUnixProcess children, and #update: them accordingly.!

!ExternalWindowsOSProcess commentStamp: '<historical>' prior: 0!
I represent an external Windows process other than the process in which this Squeak is executing. I maintain information about the state of the external process during and after the lifetime of the process. In particular, I hold the exit status of the process after it completes execution. When the external process changes state (e.g. it exits), the VM signals a Squeak semaphore. A singleton WindowsOSProcessAccessor maintains a process which waits on the semaphore, and sends a changed: #childProcessStatus message to itself, thereby notifying its dependent WindowsOSProcess (a singleton) to check the status of all its ExternalWindowsOSProcess children, and #update: them accordingly.!

!ThisOSProcess commentStamp: '<historical>' prior: 0!
I represent the operating system process in which the Squeak VM is currently running. My subclasses implement system specific features for Unix, Windows, MacOS, or other operating systems by collaborating with corresponding subclasses of OSProcessAccessor to provide primitive access to the external operating system.
!

!MacProcess commentStamp: '<historical>' prior: 0!
I represent a Macintosh operating system process, such as the process in which the Squeak VM is currently running. I collaborate with an instance of MacOSProcessAccessor to provide primitive access to the external operating system. My instance variables are maintained as a convenience to allow inspection of an OSProcess. Access to these variables should always be done with my accessor methods, which update the instance variables by querying my MacOSProcessAccessor.!

!OS2Process commentStamp: '<historical>' prior: 0!
I represent an OS2 operating system process, such as the process in which the Squeak VM is currently running. I collaborate with an instance of OS2OSProcessAccessor to provide primitive access to the external operating system. My instance variables are maintained as a convenience to allow inspection of an OSProcess. Access to these variables should always be done with my accessor methods, which update the instance variables by querying my OS2OSProcessAccessor.!

!RiscOSProcess commentStamp: '<historical>' prior: 0!
I represent an Acorn RiscOS operating system task, such as the task in which the Squeak VM is currently running. I collaborate with a singleton instance of RiscOSProcessAccessor to provide primitive access to the external operating system. My instance variables are maintained as a convenience to allow inspection of a RiscOSProcess. Access to these variables should always be done with my accessor methods, which update the instance variables by querying my RiscOSProcessAccessor.
!

!UnixProcess commentStamp: '<historical>' prior: 0!
I represent the Unix operating system process in which this Squeak session is running. I collaborate with an instance of UnixOSProcessAccessor to provide access to the external operating system. My instance variables are updated when my process accessor changes, allowing them to be monitored with a Smalltalk inspector.
!

!WindowsProcess commentStamp: 'dtl 9/25/2005 16:31' prior: 0!
I represent a Windows operating system process, such as the process in which the Squeak VM is currently running. I collaborate with an instance of WindowsOSProcessAccessor to provide primitive access to the external operating system. My instance variables are maintained as a convenience to allow inspection of an OSProcess. Access to these variables should always be done with my accessor methods, which update the instance variables by querying my WindowsOSProcessAccessor.

My process ID and process handle (a Win32 HANDLE) are held by my pid and processHandle variables. The main thread for this process is held by my mainThread variable.

Standard input, output, and error streams are available, and my be used when the console is open (WindowsProcess>>openConsole). They can also be reassigned to file streams (WindowsOSProcessAccessor>>setStdOut:).

When external processes are created, they are added to my allMyChildren collection, and a thread is created to wait for any of them to exit. This thread is held by my childWatcherThread instance variable while the thread is active, and is also added to my threads collection.

Whenever a child process exits, the childWatcherThread will signal a Semaphore (a Smalltalk Semaphore, not a Windows semaphore), then exit. A Squeak process in my processAccessor waits on this Semaphore, and sends an 'update: #childProcessStatus' message to me. In response to this, I update the status of my active child processes, one or more of which will have exited. If any of my child processes are still active, I set a new childWatcherThread to wait for them to exit.

Note that some Windows applications will exit their main process after creating another application process. These applications will appear to Squeak as if they have exited immediately, even though the application is running.!

!UnixProcessExitStatus commentStamp: 'dtl 8/23/2012 22:48' prior: 0!
A UnixProcessExitStatus represents the exit status of a unix process. This is an integer bit field answered by the wait() system call that contains information about exit status of the process. The meaning of the bit field varies according to the cause of process exit. 

Following a normal process exit, the status may be decoded to provide a small positive integer value in the range 0 - 255, which is the value that is presented by a unix shell as the exit status of a program. If terminated by a signal, the corresponding value is the signal number of the signal that caused process exit.

UnixExitStatus decodes the process exit status in a manner compatible with a typical GNU unix implementation. It is not guaranteed to be portable and may produce misleading results on other unix systems.
!

!WindowsThread commentStamp: '<historical>' prior: 0!
I represent a thread of execution within a Windows process. May threadID is a unique
identifier for the thread, and my handle is a Windows HANDLE to the thread. My handle
should be closed when the thread exits.!

!AioEventHandlerExample commentStamp: 'dtl 7/5/2003 18:38' prior: 0!
Demonstrate asynchronous read handers for file streams, OS pipes, standard input, and sockets. See class category "examples". Some examples require OSProcess.!

!OSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:41'!
aioModuleName
	"Answer a string containing the module name string for the AIO plugin."

	"OSProcess accessor aioModuleName"

	^ self subclassResponsibility! !

!OSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:41'!
aioVersionString
	"Answer a string containing the version string for the AIO plugin."

	"OSProcess accessor aioVersionString"

	^ self subclassResponsibility! !

!OSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:41'!
osppModuleName
	"Answer a string containing the module name string for the OSPP plugin."

	"OSProcess accessor osppModuleName"

	^ self subclassResponsibility! !

!OSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:41'!
osppModuleVersionString
	"Answer a string containing the version string for the OSPP plugin."

	"OSProcess accessor osppModuleVersionString"

	^ self subclassResponsibility! !

!OSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:41'!
xdcpModuleName
	"Answer a string containing the module name string for the display control plugin."

	"OSProcess accessor xdcpModuleName"

	^ self subclassResponsibility! !

!OSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:42'!
xdcpVersionString
	"Answer a string containing the version string for the display control plugin."

	"OSProcess accessor xdcpVersionString"

	^ self subclassResponsibility! !

!OSProcessAccessor methodsFor: 'testing' stamp: 'dtl 10/18/2001 22:58'!
canAccessSystem
	"Answer true if it is possible to access the external process. Concrete subclasses should
	know how to answer true."

	^ false
! !

!OSProcessAccessor methodsFor: 'testing' stamp: 'dtl 2/14/2004 11:43'!
canForwardExternalSignals
	"Answer true if it is possible to forward OS signals to a Smalltalk Semaphore."

	^ false
! !

!OSProcessAccessor methodsFor: 'testing' stamp: 'dtl 8/8/2002 15:13'!
handlesOsSignals
	"True if OS signals can be handled and forwarded to the image"

	^ false! !

!OSProcessAccessor methodsFor: 'file lock registry' stamp: 'dtl 4/9/2005 22:44'!
canAcquireLock: anOSFileLockDescriptor
	"Answer true if the file lock cache will permit fileLock to be acquired. This method
	does not guarantee that the underlying OS will grant the lock."

	^ (self fileLockRegistry anySatisfy:
			[:ld | ld isActive and: [ld conflictsWith: anOSFileLockDescriptor]]) not! !

!OSProcessAccessor methodsFor: 'file lock registry' stamp: 'dtl 3/5/2005 13:21'!
emulateWin32FileLocking
	"Answer the current value of this preference"

	^ EmulateWin32FileLocking! !

!OSProcessAccessor methodsFor: 'file lock registry' stamp: 'dtl 9/24/2009 21:36'!
fileLockRegistry

	^ FileLockRegistry ifNil: [FileLockRegistry _ Set new]
! !

!OSProcessAccessor methodsFor: 'file lock registry' stamp: 'dtl 3/5/2005 13:06'!
register: fileRegionLock
	"If an object equal to fileRegionLock exists in the registry, answer it. Otherwise, add
	fileRegionLock to the registry and answer fileRegionLock."

	^ (self fileLockRegistry like: fileRegionLock)
		ifNil: [self fileLockRegistry add: fileRegionLock]
! !

!OSProcessAccessor methodsFor: 'file lock registry' stamp: 'dtl 4/10/2005 15:05'!
registeredLocksForFile: aFileStream
	"Answer all lock descriptors associated with aFileStream"

	^ self fileLockRegistry select: [:ea | ea fileStream = aFileStream]
! !

!OSProcessAccessor methodsFor: 'file lock registry' stamp: 'dtl 1/20/2018 19:20:59'!
removeInactiveLocks
	"Go through the lock cache and remove any that have been left
	behind after their streams were closed."

	^ self fileLockRegistry copy do: [:ea | ea isActive ifFalse: [self fileLockRegistry remove: ea]]! !

!OSProcessAccessor methodsFor: 'file lock registry' stamp: 'dtl 3/5/2005 13:07'!
unregister: fileRegionLock
	"If an object equal to fileRegionLock exists in the registry, remove it and
	answer the object. Otherwise answer nil."


	^ self fileLockRegistry remove: fileRegionLock ifAbsent: [nil]
! !

!OSProcessAccessor methodsFor: 'accessing' stamp: 'dtl 4/7/2007 10:19'!
canObtainSessionIdentifierFromPlugin

	^ canObtainSessionIdentifierFromPlugin
		ifNil: [canObtainSessionIdentifierFromPlugin _ self primGetSession notNil]! !

!OSProcessAccessor methodsFor: 'accessing' stamp: 'dtl 2/27/2015 19:54'!
grimReaper
	"Answer the value of grimReaper"

	^ grimReaper! !

!OSProcessAccessor methodsFor: 'accessing' stamp: 'dtl 2/27/2015 19:54'!
grimReaper: anObject
	"Set the value of grimReaper"

	grimReaper _ anObject! !

!OSProcessAccessor methodsFor: 'accessing' stamp: 'dtl 3/2/2002 08:32'!
sessionIdentifier

	^ sessionIdentifier ifNil: [sessionIdentifier _ self getSessionIdentifier]
! !

!OSProcessAccessor methodsFor: 'session identification' stamp: 'dtl 8/20/2022 01:22'!
getSessionIdentifier
	"Answer the unique identifier for this Squeak session"

	^ self primGetSession.
! !

!OSProcessAccessor methodsFor: 'session identification' stamp: 'dtl 1/20/2018 19:21:57'!
getSessionIdentifierFromSourcesFile
	"Deduce the session identifier from an existing open FileStream on the sources file.
	This is an unreliable method, because it assumes knowledge of the internal structure
	of the SQFile data structure.

	Deprecated:
	As of approximately Squeak 3.8 and beyond, the session id has been moved to the
	first slot of the data structure. This method will not work for a Squeak VM beyond
	that point, and will not work for any 64 bit VM. However, an reliable means of
	obtaining sessionID is now available (#getSessionIdentifier), so this method is retained
	only for backwards compatibility to allow OSPP to be built on an older VMMaker."

	"OSProcess accessor getSessionIdentifierFromSourcesFile"

	| s id |
	s _ SourceFiles first.
	s ifNil: [^ nil].
	^ (Smalltalk hasClassNamed: #IOHandle)
		ifTrue: [(s ioHandle perform: #getHandle) copyFrom: 5 to: 8]
		ifFalse: [(id _ s fileID) ifNotNil: [id copyFrom: 5 to: 8]]
! !

!OSProcessAccessor methodsFor: 'session identification' stamp: 'dtl 3/2/2002 09:07'!
primGetSession
	"Subclasses should override if they know how to obtain the session identifier."

	^ nil! !

!OSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 9/25/2005 16:07'!
getStdErrHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard error for the
	OS process in which I am currently executing."

	^ self subclassResponsibility
! !

!OSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 9/25/2005 16:07'!
getStdInHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard input for the
	OS process in which I am currently executing."

	^ self subclassResponsibility
! !

!OSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 9/25/2005 16:06'!
getStdOutHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard output for the
	OS process in which I am currently executing."

	^ self subclassResponsibility
! !

!OSProcessAccessor methodsFor: 'private - IOHandle' stamp: 'dtl 1/20/2018 19:22:28'!
handleFromAccessor: aByteArrayOrIOAccessor

	UseIOHandle
		ifTrue: [aByteArrayOrIOAccessor isNil
			ifTrue: [^ nil]
			ifFalse: [^ aByteArrayOrIOAccessor perform: #asSQFileStruct]]
		ifFalse: [^ aByteArrayOrIOAccessor]! !

!OSProcessAccessor methodsFor: 'private - IOHandle' stamp: 'dtl 1/3/2004 21:21'!
handleFromFileStream: aFileStream

	^ UseIOHandle
		ifTrue: [self handleFromAccessor: aFileStream ioHandle]
		ifFalse: [aFileStream fileID]
! !

!OSProcessAccessor methodsFor: 'private - IOHandle' stamp: 'dtl 1/20/2018 19:18:06'!
ioAccessorFromSQFile: aByteArray
	"Answer an object which represents an IO channel. If IOHandle is present in
	this image, use it; otherwise just answer aByteArray."

	UseIOHandle
		ifTrue: [^ (Smalltalk at: #IOHandle) perform: #newFromSqFileStruct: with: aByteArray]
		ifFalse: [^ aByteArray]! !

!OSProcessAccessor methodsFor: 'initialize - release' stamp: 'dtl 2/27/2015 19:51'!
initialize

	canObtainSessionIdentifierFromPlugin _ nil.
	self canObtainSessionIdentifierFromPlugin.
	sessionIdentifier _ nil.
	self sessionIdentifier.
! !

!OSProcessAccessor methodsFor: 'initialize - release' stamp: 'ThierryGoubier 7/27/2017 22:37'!
newPid
	"This image is now being run in a new VM process with different pid. Pause the handling
	of child processes, and remove references to child processes that no longer pertain to
	the current VM process."

	grimReaper notNil
		ifTrue: [ grimReaper terminate.
			grimReaper _ nil ].
	self changed: #pid.
	self restartChildWatcherProcess! !

!OSProcessAccessor methodsFor: 'initialize - release' stamp: 'dtl 2/28/2015 18:31'!
restartChildWatcherProcess
	self subclassResponsibility! !

!OSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 8/19/2022 19:10'!
isAtEndOfFile: ioHandle 
	"Answer whether the file represented by ioHandle is at end of file, as determined
	by a call to feof(). This is different from StandardFileStream>>primAtEnd: which answers
	true if the file pointer is at the end of the file, but which does not call feof() to
	determine that an end of file condition has occurred. The difference is significant
	if aSqFileStruct represents a pipe or a device file, which may not be positionable
	in the sense of a conventional disk file."
	^ (self primTestEndOfFileFlag: ioHandle) = true! !

!OSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 4/8/2007 10:49'!
primTestEndOfFileFlag: aSqFileStruct
	"Answer whether the file represented by aSqFileStruct is at end of file, as determined
	by a call to feof(). This is different from StandardFileStream>>primAtEnd: which answers
	true if the file pointer is at the end of the file, but which does not call feof() to
	determine that an end of file condition has occurred. The difference is significant
	if aSqFileStruct represents a pipe or a device file, which may not be positionable
	in the sense of a conventional disk file."

	^ self subclassResponsibility
! !

!OSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 1/31/2020 00:14'!
spaceFor: bytesRequired inDirectoryPath: dirPath
	"Given a file size requirement in bytes, and a path specifying a location
	within a file system, answer true if that file system has sufficient free
	space to save a file of that size. The file size parameter must be a
	SmallInteger (not LargePositiveInteger) and the directory path is a string.
	This primitive is entended as a guard to protect against saving an image
	file when the file system is full."

	"OSProcess accessor spaceFor: 16000000 inDirectoryPath: '/home' "
	"OSProcess accessor spaceFor: 16000000 inDirectoryPath: '/proc' "
	"OSProcess accessor spaceFor: 16000000 inDirectoryPath: 'INVALID PATH' "

	^ self primSpaceFor: bytesRequired InDirectoryPath: dirPath
! !

!OSProcessAccessor methodsFor: 'platform identification' stamp: 'dtl 8/24/2003 09:18'!
isResponsibleForThisPlatform
	"Answer true is this is an instance of the class which is responsible for representing
	the OS process for the Squeak VM running on the current platform. A false answer is
	usually the result of running the image on a different platform and VM."

	^ self class isResponsibleForThisPlatform! !

!OSProcessAccessor methodsFor: 'pipe open' stamp: 'ThierryGoubier 7/19/2017 22:00'!
makePipeHandles
	"Create a pipe, and answer an array of two IO accessors for the pipe 
	reader and writer."

	"OSProcess accessor makePipeHandles"

	| p |
	p _ self primCreatePipe.
	p isNil
		ifFalse: [ ^ p collect: [ :e | self ioAccessorFromSQFile: e ] ].
	^ nil! !

!OSProcessAccessor methodsFor: 'file control' stamp: 'dtl 2/11/2001 15:37'!
setNonBlocking: anIOHandle
	"Convert anIOHandle to an SQFile data structure and call primitive to set it non-blocking."

	^ self subclassResponsibility
! !

!OSProcessAccessor methodsFor: 'system configuration access' stamp: 'dtl 10/17/2020 16:56'!
confstr: optionIndex
	"Answer the string result of confstr() for the supplied option index, an
	integer value defined in unistd.h. On some platforms, the available index
	values are defined in /usr/include/bits/confname.h"

	"OSProcess accessor confstr: 0"

	^self primConfstr: optionIndex! !

!OSProcessAccessor methodsFor: 'system configuration access' stamp: 'dtl 10/17/2020 23:44'!
fpathconf: fileStream index: optionIndex
	"Answer the result of fpathconf() for the file descriptor fo a FileStream,
	and the supplied option index, an integer value defined in unistd.h.
	On some platforms, the available index values are defined in
	/usr/include/bits/confname.h"

	"OSProcess accessor fpathconf: SourceFiles first index: 0"

	^self primFpathconf: fileStream fileID index: optionIndex
! !

!OSProcessAccessor methodsFor: 'system configuration access' stamp: 'dtl 10/18/2020 11:33'!
pathconf: path index: optionIndex
	"Answer the result of pathconf() for file path string and the
	supplied option index, an integer value defined in unistd.h. On
	some platforms, the available index values are defined in
	/usr/include/bits/confname.h"

	"OSProcess accessor pathconf: '/bin/bash' index: 0"

	^self primPathconf: path index: optionIndex
! !

!OSProcessAccessor methodsFor: 'system configuration access' stamp: 'dtl 10/17/2020 16:32'!
sysconf: optionIndex
	"Answer the result of sysconf() for the supplied option index, and integer
	value typically defined in unistd.h or limits.h"

	"OSProcess accessor sysconf: 0"

	^self primSysconf: optionIndex
! !

!OSProcessAccessor class methodsFor: 'concrete subclasses' stamp: 'dtl 3/5/2005 12:04'!
concreteClass

	"OSProcessAccessor concreteClass"

	^ self subclasses
		detect: [:c | c isResponsibleForThisPlatform]
		ifNone: [self notify: self printString,
					': No concrete class implementation available for system type ',
					OSProcess platformName printString.
				nil]

! !

!OSProcessAccessor class methodsFor: 'initialize-release' stamp: 'dtl 3/5/2005 13:24'!
emulateWin32FileLocking: trueOrFalse
	"This is a preference that controls whether file locking will attempt to emulation
	Win32 behavior, in which a lock request will fail if the requested region overlaps
	a region for which there is an existing lock. This behavior is valid only for locks
	managed within a single Squeak image, and will not produce the expected results
	for a Squeak image cooperating with another Squeak image, or with some other
	external program.

	Use of the Win32 emulation may result in performance penalties for an application
	that performs a large number of lock requests, such as a database."

	"self emulateWin32FileLocking: true"
	"self emulateWin32FileLocking: false"

	EmulateWin32FileLocking _ trueOrFalse
! !

!OSProcessAccessor class methodsFor: 'initialize-release' stamp: 'dtl 8/20/2022 00:58'!
initialize

	"OSProcessAccessor initialize"

	ThisOSProcessAccessor _ nil.
	self emulateWin32FileLocking: false.
	self allSubInstances do: [:e | e finalize]
! !

!OSProcessAccessor class methodsFor: 'instance creation' stamp: 'dtl 12/14/2013 09:15'!
forThisOSProcess
	"Answer a single instance corresponding to the OS process in which this 
	Smalltalk image is executing."

	"OSProcessAccessor forThisOSProcess"

	ThisOSProcessAccessor
		ifNotNil: [ThisOSProcessAccessor isResponsibleForThisPlatform
					ifTrue:
						["Common case, platform has not changed"
						^ThisOSProcessAccessor ]
					ifFalse:
						["We are running on a different platform, so start a new accessor"
						ThisOSProcessAccessor changed: #invalidProcessAccessor.
						ThisOSProcessAccessor finalize]].
	^ ThisOSProcessAccessor _ self concreteClass basicNew initialize
! !

!OSProcessAccessor class methodsFor: 'instance creation' stamp: 'dtl 3/2/2002 08:29'!
new

	self inform: 'use OSProcessAccessor>>forThisOSProcess to create or obtain the OSProcess instance for this Smalltalk session.'.
	^ nil! !

!OSProcessAccessor class methodsFor: 'platform identification' stamp: 'dtl 8/24/2003 09:17'!
isResponsibleForThisPlatform
	"Answer true if an instance of this class is responsible for representing the
	OS process for the Squeak VM running on the current platform."

	^ self subclassResponsibility! !

!MacOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 2/23/2002 05:57'!
primGetSession
	"Answer the unique identifier for this session of Smalltalk running in this OS Process."

	^ nil
! !

!MacOSProcessAccessor class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:44'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for the
	Squeak VM running on the current platform."

	^ OSProcess isNonUnixMac
! !

!OS2OSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 2/23/2002 05:56'!
primGetSession
	"Answer the unique identifier for this session of Smalltalk running in this OS Process."

	^ nil
! !

!OS2OSProcessAccessor class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:47'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for the
	Squeak VM running on the current platform."

	^ OSProcess isOS2
! !

!RiscOSProcessAccessor methodsFor: 'fork and exec' stamp: 'dtl 1/6/2001 23:15'!
primForkAndExec: executableFile
	withArgs: anArrayOfArgumentStrings
	argCount: numberOfArgumentStrings
	withEnv: anArrayOfEnvironmentStrings
	envCount: numberOfEnvironmentStrings
	stdIn: inputFileHandle
	stdOut: outputFileHandle
	stdErr: errorFileHandle
	"Parameters are expected to have been properly prepared by the caller,
	including string values which are to be null terminated strings.
	In other words, all strings should have (Character null) as the
	last element in the string."! !

!RiscOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 2/23/2002 05:56'!
primGetSession
	"Answer the unique identifier for this session of Smalltalk running in this OS Process."

	^ nil
! !

!RiscOSProcessAccessor class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:48'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for the
	Squeak VM running on the current platform."

	^ OSProcess isRiscOS
! !

!UnixOSProcessAccessor methodsFor: 'printing' stamp: 'dtl 9/10/2000 10:16'!
printOn: aStream
	"In English, say 'a Unix' rather than 'an Unix'. Therefore do not use super printOn, which
	treats $U as a vowel."

	aStream
		nextPutAll: 'a ';
		nextPutAll: self class name;
		nextPutAll: ' on pid ';
		nextPutAll: self primGetPid printString
! !

!UnixOSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:43'!
aioModuleName
	"Answer a string containing the module name string for the AIO plugin."

	"OSProcess accessor aioModuleName"

	^ self primAioPluginModuleName
! !

!UnixOSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:44'!
aioVersionString
	"Answer a string containing the version string for the AIO plugin."

	"OSProcess accessor aioVersionString"

	^ self primAioPluginVersionString
! !

!UnixOSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 11:48'!
osppModuleName
	"Answer a string containing the module name string for the OSPP plugin."

	"OSProcess accessor osppModuleName"

	^ self primOSProcessPluginModuleName
! !

!UnixOSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 11:49'!
osppModuleVersionString
	"Answer a string containing the version string for the OSPP plugin."

	"OSProcess accessor osppModuleVersionString"

	^ self primOSProcessPluginModuleVersionString
! !

!UnixOSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:45'!
xdcpModuleName
	"Answer a string containing the module name string for the display control plugin."

	"OSProcess accessor xdcpModuleName"

	^ self primXDisplayControlPluginModuleName
! !

!UnixOSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 10:46'!
xdcpVersionString
	"Answer a string containing the version string for the display control plugin."

	"OSProcess accessor xdcpVersionString"

	^ self primXDisplayControlPluginModuleVersionString
! !

!UnixOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 10/7/2000 13:39'!
canAccessChildProcess: anExternalProcess
	"Is the child process still there? Maybe not if we have restarted the image
	and anExternalProcess refers to a process which died while we were not
	watching."

	^ self primCanReceiveSignals: anExternalProcess pid! !

!UnixOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 9/9/2000 15:45'!
canAccessSystem
	"Answer true if it is possible to access the external process, else false. Failure
	to access the external process is probably due to lack of a UnixOSProcessPlugin
	module."

	^ self primGetPid notNil
! !

!UnixOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 8/30/2003 18:43'!
canControlXDisplay
	"True if the XDisplayControlPlugin is accessible. Older versions of OSProcess relied on
	the X display control to be embedded in the OSProcessPlugin module. This has been
	moved to a separate display control plugin to allow support of non-X platforms and
	other display media on Unix platforms (OS X)."

	"OSProcess accessor canControlXDisplay"

	^ self primXDisplayControlPluginModuleName notNil
! !

!UnixOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 2/14/2004 12:17'!
canForwardExternalSignals
	"Answer true if it is possible to forward OS signals to a Smalltalk Semaphore."

	^ true
! !

!UnixOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 8/8/2002 15:13'!
handlesOsSignals
	"True if OS signals can be handled and forwarded to the image"

	^ true! !

!UnixOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 3/25/2001 15:34'!
sizeOfInt
	"Size of an integer on this machine with this C compiler."

	^ self primSizeOfInt! !

!UnixOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 3/25/2001 20:55'!
sizeOfPointer
	"Size of a void pointer on this machine with this C compiler."

	^ self primSizeOfPointer! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 12/18/2022 11:49'!
chDir: pathString
	"Change current working directory. The $PWD environment variable is not updated (but
	see UnixProcess>>chDir:). Answer nil for success, or an error message."

	"OSProcess accessor chDir: '/tmp'"
	"OSProcess accessor chDir: '/no/such/path'"
	"OSProcess accessor chDir: FileDirectory default pathName"

	| result message |
	result _ self primChdir: pathString asByteArray.
	result isNil
		ifTrue:
			[^ nil]
		ifFalse:
			[message _ self primErrorMessageAt: result.
			^ message]
! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 12/18/2022 11:49'!
environmentAt: aSymbolOrString
	"Get an environment variable from the external OS process."

	"OSProcess accessor environmentAt: 'PATH'"

	^ self primEnvironmentAtSymbol: aSymbolOrString asByteArray
! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 12/18/2022 12:11'!
environmentAt: aSymbolOrString put: aString
	"Add or update an environment variable in the external OS process. Convert aSymbol
	and aString into a KEY=value string and pass this to the OS process environment.
	Standard C libraries provide a putenv() function for this purpose, with a parameter
	in the form KEY=value. Note: Maintain a reference to the return value, see note in
	primitivePutEnv."

	"OSProcess accessor environmentAt: 'AAAA' put: 'this is the value of AAAA'"
	"OSProcess accessor environmentAt: 'AAAA' put: nil"
	"OSProcess accessor environmentAt: 'AAAA'"

	aString isNil
		ifTrue:
			[^ self primUnsetEnv: aSymbolOrString]
		ifFalse:
			[ | newStringBytes |
			newStringBytes _ self environmentPut: (aSymbolOrString asString, '=', aString) asByteArray.
			^ newStringBytes asString]
! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 1/20/2019 11:21'!
primArgumentAsBytesAt: index
	"Answer the argument string in the argument OS process argument list at position index.
	In Unix, the first element of the list is the program name, and any additional elements
	of the list are optional command line arguments passed to the program. This convention
	may be simulated by the C runtime libraries on other operating systems, but argument
	list handling should be assumed to be operating system dependent."

	<primitive: 'primitiveArgumentAtAsBytes' module: 'UnixOSProcessPlugin'>
	self primitiveFailed! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 7/22/2000 17:19'!
primArgumentAt: index
	"Answer the argument string in the argument OS process argument list at position index.
	In Unix, the first element of the list is the program name, and any additional elements
	of the list are optional command line arguments passed to the program. This convention
	may be simulated by the C runtime libraries on other operating systems, but argument
	list handling should be assumed to be operating system dependent."

	<primitive: 'primitiveArgumentAt' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 1/20/2019 12:26'!
primEnvironmentAsBytesAt: index
	"Answer the environment string at index position in the OS process environment list.
	This returns a 'KEY=value' string, which the caller is expected to parse into #KEY
	and 'value' to be stored an environment dictionary."

	<primitive: 'primitiveEnvironmentAtAsBytes' module: 'UnixOSProcessPlugin'>
	self primitiveFailed! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 7/22/2000 17:20'!
primEnvironmentAt: index
	"Answer the environment string at index position in the OS process environment list.
	This returns a 'KEY=value' string, which the caller is expected to parse into #KEY
	and 'value' to be stored an environment dictionary."

	<primitive: 'primitiveEnvironmentAt' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 3/31/2001 11:03'!
primErrorMessageAt: anInteger
	"Answer an error message string from the sys_errlist array, indexed by anInteger."

	"OSProcess accessor primErrorMessageAt: 0"
	"OSProcess accessor primErrorMessageAt: 100"
	"OSProcess accessor primErrorMessageAt: 1000"
	"OSProcess accessor primErrorMessageAt: -1"

	<primitive: 'primitiveErrorMessageAt' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 7/22/2000 17:20'!
primGetCurrentWorkingDirectory
	"Call getcwd() to get the current working directory."

	"OSProcess thisOSProcess processAccessor primGetCurrentWorkingDirectory"

	<primitive: 'primitiveGetCurrentWorkingDirectory' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 1/20/2019 12:27'!
primGetCurrentWorkingDirectoryAsBytes
	"Call getcwd() to get the current working directory."

	"OSProcess thisOSProcess processAccessor primGetCurrentWorkingDirectoryAsBytes"

	<primitive: 'primitiveGetCurrentWorkingDirectoryAsBytes' module: 'UnixOSProcessPlugin'>
	self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 4/30/2001 21:07'!
primGetEGid
	"Answer the effective group ID for the OS process in which I am currently executing."

	"OSProcess accessor primGetEGid"

	<primitive: 'primitiveGetEGid' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 4/30/2001 21:01'!
primGetEUid
	"Answer the effective user ID for the OS process in which I am currently executing."

	"OSProcess accessor primGetEUid"

	<primitive: 'primitiveGetEUid' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 4/30/2001 21:01'!
primGetGid
	"Answer the group ID for the OS process in which I am currently executing."

	"OSProcess accessor primGetGid"

	<primitive: 'primitiveGetGid' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 3/15/2007 19:43'!
primGetPGid: pid
	"Answer the process group ID of the process identified by pid"

	<primitive: 'primitiveGetPGid' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 3/15/2007 19:39'!
primGetPGrp
	"Answer the process group ID of this OS process"

	<primitive: 'primitiveGetPGrp' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 7/22/2000 17:20'!
primGetPPid
	"Answer the OS process ID for the parent process of the OS process in which I am
	currently executing."

	<primitive: 'primitiveGetPPid' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 7/22/2000 17:20'!
primGetPid
	"Answer the OS process ID for the OS process in which I am currently executing."

	<primitive: 'primitiveGetPid' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 3/2/2002 08:15'!
primGetSession
	"Answer the unique identifier for this session of Smalltalk running in this OS Process."

	"OSProcess accessor primGetSession"

	<primitive: 'primitiveGetSession' module: 'UnixOSProcessPlugin'>

	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 4/30/2001 21:00'!
primGetUid
	"Answer the user ID for the OS process in which I am currently executing."

	"OSProcess accessor primGetUid"

	<primitive: 'primitiveGetUid' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 3/15/2007 19:47'!
primSet: pid pGid: processGroupId
	"Set the process group ID of the process identified by pid to a new process
	group ID."

	<primitive: 'primitiveSetPGid' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 3/15/2007 19:40'!
primSetPGrp
	"Set a new process group for this OS process. Newly created child processes
	will be members of the new process group."

	<primitive: 'primitiveSetPGrp' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 3/18/2007 10:50'!
primSetSid
	"Quoted from Linux man pages:
	setsid() creates a new session if the calling process is not a process group leader.
	The calling process is the leader of the new session, the process  group  leader  of
	the new process group, and has no controlling tty.  The process group ID and session
	ID of the calling process are set to the PID of the calling  process.   The  calling
	process  will be the only process in this new process group and in this new session."

	<primitive: 'primitiveSetSid' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 9/10/2000 11:59'!
putPath: aString
	"Convenience method. Set the environment PATH variable to aString."

	| pathString |
	pathString _ 'PATH=', aString, ((Character null) asString). 
	^ self environmentPut: pathString




! !

!UnixOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 12/18/2022 12:16'!
realpath: pathString
	"Get the real path for pathString from the external OS process."

	"OSProcess accessor realpath: '/tmp'"
	"OSProcess accessor realpath: OSProcess defaultPathString"
	"OSProcess accessor realpath: '/bogus/path/name'"

	^ self primRealpath: pathString asByteArray
! !

!UnixOSProcessAccessor methodsFor: 'signal print stack trace' stamp: 'dtl 7/20/2010 00:06'!
clearPrintAllStacksOnSigUsr1
	"Clear any signal handler in the VM that was set to print all call stacks on
	console output when a SIGUSR1 is received. Restores default SIGUSR1
	handler, which may result in exiting the VM on this signal. Answer an
	identifier for the previously defined signal handler or nil if no handler
	had been set."

	^self primForwardSignal: self primSigUsr1Number toSemaphore: nil	
! !

!UnixOSProcessAccessor methodsFor: 'signal print stack trace' stamp: 'dtl 7/20/2010 00:07'!
setPrintAllStacksOnSigUsr1
	"Set a signal handler in the VM that will print all call stacks on console output
	whenever a SIGUSR1 is received. Answer an identifier for the previously
	defined signal handler or nil if this signal handler has already been set."
	
	"OSProcess accessor setPrintAllStacksOnSigUsr1"
	"OSProcess accessor clearPrintAllStacksOnSigUsr1"

	^self primPrintAllStacksOnSignal: self primSigUsr1Number	
! !

!UnixOSProcessAccessor methodsFor: 'file control' stamp: 'dtl 3/2/2009 22:42'!
closeUnixFileNumber: unixFileDescriptor
	"Close the file associated with unixFileDescriptor, a small integer value."

	<primitive: 'primitiveUnixFileClose' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'file control' stamp: 'dtl 8/19/2022 19:11'!
flushExternalStream: ioHandle
	"Convert ioHandle to an SQFile data structure and call primitive to flush the
	external I/O stream."

	^ self primSQFileFlush: ioHandle
! !

!UnixOSProcessAccessor methodsFor: 'file control' stamp: 'dtl 8/19/2022 19:07'!
setBlocking: ioHandle
	"Convert ioHandle to an SQFile data structure and call primitive to set for blocking I/O."

	^ self primSQFileSetBlocking: ioHandle
! !

!UnixOSProcessAccessor methodsFor: 'file control' stamp: 'dtl 8/19/2022 19:07'!
setNonBlocking: ioHandle
	"Convert ioHandle to an SQFile data structure and call primitive to set it non-blocking."

	^ self primSQFileSetNonBlocking: ioHandle! !

!UnixOSProcessAccessor methodsFor: 'file control' stamp: 'dtl 8/19/2022 19:04'!
setUnbuffered: ioHandle
	"Convert ioHandle to an SQFile data structure and call primitive to set unbuffered I/O."

	^ self primSQFileSetUnbuffered: ioHandle! !

!UnixOSProcessAccessor methodsFor: 'file control' stamp: 'dtl 8/19/2022 19:04'!
unixFileNumber: ioHandle
	"Answer the integer Unix file number corresponding to ioHandle."

	^ ioHandle ifNotNil: [self primUnixFileNumber: ioHandle]
! !

!UnixOSProcessAccessor methodsFor: 'debugging' stamp: 'dtl 11/19/2006 09:02'!
currentSigHandlerAddress: signalNumber
	"Answer the current machine address of the signal handler for signalNumber,
	expressed as a ByteArray.

	Temporarily set a signal forwarded for signalNumber, and remember the
	machine address of the prior signal handler. Restore the signal handler
	to its previous value, and answer the machine address of the handler."

	"OSProcess accessor currentSigHandlerAddress: OSProcess accessor primSigIntNumber"

	| sema index previousHandlerAddress |
	sema _ Semaphore new.
	index _ Smalltalk registerExternalObject: sema.
	(previousHandlerAddress _ self primForwardSignal: signalNumber toSemaphore: index)
		ifNil:
			[Smalltalk unregisterExternalObject: sema.
			^ self error: 'could not forward signal number ', signalNumber asString].
	self restoreSignal: signalNumber.
	^ previousHandlerAddress! !

!UnixOSProcessAccessor methodsFor: 'debugging' stamp: 'dtl 11/19/2006 09:05'!
listSigHandlerAddressesOnConsole

	"OSProcess accessor listSigHandlerAddressesOnConsole"

	(0 to: 67) do: [:sigNum | | prev |
		[prev _ OSProcess accessor currentSigHandlerAddress: sigNum.
		OSProcess debugMessage: 'signal ', sigNum asString,' hander is ', prev printString]
			on: Error
			do: [:ex | OSProcess debugMessage: ex printString]]
! !

!UnixOSProcessAccessor methodsFor: 'debugging' stamp: 'dtl 11/19/2006 09:07'!
listSigHandlerAddressesOnTranscript

	"OSProcess accessor listSigHandlerAddressesOnTranscript"

	Transcript cr.
	(0 to: 67) do: [:sigNum | | prev |
		[prev _ OSProcess accessor currentSigHandlerAddress: sigNum.
		Transcript show: 'signal ', sigNum asString,' hander is ', prev printString; cr]
			on: Error
			do: [:ex | Transcript show: ex printString; cr]]
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 9/10/2000 09:48'!
environmentPut: aString
	"Add or update an environment variable in the external OS process using a 'KEY=value'
	string. Create a null terminated string for use by the external putenv() call in a pluggable
	primitive."

	"OSProcess thisOSProcess processAccessor environmentPut: 'SOMEKEY=somevalue'"

	| cString |
	cString _ aString, (Character null) asString.
	^ self primPutEnv: cString
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/1/2005 10:37'!
primAioPluginModuleName
	"Answer a string containing the module name string for the AIO plugin."

	"OSProcess accessor primAioPluginModuleName"

	<primitive: 'primitiveModuleName' module: 'AioPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/1/2005 10:38'!
primAioPluginVersionString
	"Answer a string containing the version string for the AIO plugin."

	"OSProcess accessor primAioPluginVersionString"

	<primitive: 'primitiveVersionString' module: 'AioPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 3/31/2001 11:18'!
primChdir: pathString
	"Change current working directory. Does not update the $PWD environment variable."

	<primitive: 'primitiveChdir' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/17/2020 16:56'!
primConfstr: optionIndex
	"Answer the result of confstr() for the supplied option index, and integer
	value typically defined in unistd.h or limits.h"

	<primitive: 'primitiveConfstr' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 11/28/2010 12:31'!
primCreatePipe
	"Create a pipe, and answer an array of two file handles (SQFile data structures in interp.c)
	for the pipe reader and writer."

	<primitive: 'primitiveCreatePipe' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 11/28/2010 12:32'!
primCreatePipeWithSessionIdentifier: aByteArray
	"Create a pipe, and answer an array of two file handles (SQFile data structures in interp.c)
	for the pipe reader and writer."

	<primitive: 'primitiveCreatePipeWithSessionIdentifier' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 9/10/2011 23:28'!
primDup: fileDescriptor
	"Call dup2 to duplicate a file descriptor to the next available descriptor. Answer
	the new file descriptor or -1 on failure."

	<primitive: 'primitiveDup' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 5/17/2009 21:57'!
primDup: oldFileDescriptor to: newFileDescriptor
	"Call dup2() to duplicate a file descriptor. Answer the duplicated file descriptor
	or -1 on failure."

	<primitive: 'primitiveDupTo' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 1/20/2019 12:25'!
primEnvironmentAsBytesAtSymbol: aSymbol
	"Answer a ByteArray with the value of an environment variable in
	the external OS process."

	<primitive: 'primitiveEnvironmentAtSymbolAsBytes' module: 'UnixOSProcessPlugin'>
	self primitiveFailed! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 7/22/2000 17:28'!
primEnvironmentAtSymbol: aSymbol
	"Answer the value of an environment variable in the external OS process."

	<primitive: 'primitiveEnvironmentAtSymbol' module: 'UnixOSProcessPlugin'>
	^ nil ! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 4/30/2001 05:49'!
primFileProtectionMask: aPathString
	"Call stat(2) to obtain the file protection mask for a file. Answer an Array of
	four integers representing the protection mask, or answer errno on failure. The
	protection mask is four Integers, each of which may be considered an octal digit
	(0-7), with bit values 4, 2, and 1. The first digit selects the set user ID (4) and set
	group ID (2) and save text image (1) attributes. The second digit selects permissions
	for the user who owns the file: read (4), write (2), and execute (1); the third
	selects permissions for other users in the file's group, with the same values; and
	the fourth for other users not in the file's group, with the same values."

	<primitive: 'primitiveFileProtectionMask' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 4/30/2001 20:56'!
primFileStat: aPathString
	"Call stat(2) to obtain the file protection mask for a file. Answer errno on failure,
	or on success answer an array with: UID with: GID with: protectionMask. The	
	protectionMask is an Array of four integers representing the protection mask, or
	answer errno on failure. The protection mask is four Integers, each of which may
	be considered an octal digit (0-7), with bit values 4, 2, and 1. The first digit selects
	the set user ID (4) and set group ID (2) and save text image (1) attributes. The second
	digit selects permissions for the user who owns the file: read (4), write (2), and
	execute (1); the third selects permissions for other users in the file's group, with
	the same values; and the fourth for other users not in the file's group, with the
	same values."

	<primitive: 'primitiveFileStat' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/17/2020 23:28'!
primFpathconf: fileID index: optionIndex
	"Answer the result of pathconf() for fileID, the fileID of a FileStream,
	and the supplied option index, an integer value defined in unistd.h.
	On some platforms, the available index values are defined in
	/usr/include/bits/confname.h"

	<primitive: 'primitiveFpathconf' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 7/22/2000 17:28'!
primGetStdErrHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard error for the
	OS process in which I am currently executing."

	<primitive: 'primitiveGetStdErrHandle' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 7/22/2000 17:28'!
primGetStdInHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard input for
	the OS process in which I am currently executing."

	<primitive: 'primitiveGetStdInHandle' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 7/22/2000 17:28'!
primGetStdOutHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard output for the
	OS process in which I am currently executing."

	<primitive: 'primitiveGetStdOutHandle' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 3/18/2007 10:35'!
primKill: listOfPids withSignal: signumOrNil
	"Set a list of pids to kill with signum when VM exits. If the signum
	parameter is nil, the default value of SIGTERM will be used."

	<primitive: 'primitiveKillOnExit' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 1/3/2004 19:59'!
primLockFileRegion: aSQFileStruct offset: begin length: len exclusive: flag
	"Pass a struct SQFile on the stack, and request a lock on the specified region.
	If the exclusive flag is true, then request an exclusive (F_WRLCK) lock on the
     file. Otherwise, request a shared (F_RDLCK) lock. Any number of Unix processes
     may  hold  a read lock (shared lock) on a file region, but only one process may
     hold a write lock (exclusive lock). Answer the result of the call to fcntl()."

	<primitive: 'primitiveLockFileRegion' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 6/1/2013 17:14'!
primMkdtemp: templateString
	"Call mkdtemp() to generate a uniquely named temporary directory from a template
	string. The last six characters of template must be XXXXXX and these are replaced
	with characters that make the directory name unique. The directory is then created
	with permissions 0700. Answer the new directory name."

	<primitive: 'primitiveMkdtemp' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 6/3/2013 22:33'!
primMkstemp: templateString
	"Call mkstemp() to generate a unique temporary file from a template string.
	The last six characters of template must be XXXXXX and these are replaced with
	a string that make the filename unique. Answer a SQFile descriptor for the file stream.
	
	Contents of the template string are modified, and provide the name of the newly
	created file.

	The  file  is  created  with permissions 0600, that is, read plus write for owner only.

	Sender must convert the file descriptor to a FileStream before it can be used."

	<primitive: 'primitiveMkstemp' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 6/1/2013 17:14'!
primMktemp: templateString
	"Call mktemp() to generate a unique temporary filename from a template string.
	The last six characters of template must be XXXXXX and these are replaced with
	a string that make the filename unique. The templateString must be a mutable
	string, because the primitive will change its contents.
	
	Use primitiveMkstemp to avoid file creation race condition, see man 3 mktemp."

	<primitive: 'primitiveMktemp' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/1/2005 11:48'!
primOSProcessPluginModuleName
	"Answer a string containing the module name string for the OSPP plugin."

	"OSProcess accessor primOSProcessPluginModuleName"

	<primitive: 'primitiveModuleName' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/1/2005 11:48'!
primOSProcessPluginModuleVersionString
	"Answer a string containing the version string for the OSPP plugin."

	"OSProcess accessor primOSProcessPluginModuleVersionString"

	<primitive: 'primitiveVersionString' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/17/2020 17:12'!
primPathconf: path index: optionIndex
	"Answer the result of pathconf() for file path string and the
	supplied option index, an integer value defined in unistd.h. On
	some platforms, the available index values are defined in
	/usr/include/bits/confname.h"

	<primitive: 'primitivePathconf' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 7/22/2000 17:28'!
primPutEnv: aString
	"Add or update an environment variable in the external OS process using a 'KEY=value'
	string."

	<primitive: 'primitivePutEnv' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 3/31/2001 15:44'!
primRealpath: pathString
	"Resolve pathString into a real path if possible, or answer nil."

	<primitive: 'primitiveRealpath' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 1/20/2019 12:32'!
primRealpathAsBytes: pathString
	"Resolve pathString into a real path if possible, or answer nil."

	<primitive: 'primitiveRealpathAsBytes' module: 'UnixOSProcessPlugin'>
	self primitiveFailed! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 7/22/2000 17:28'!
primSQFileFlush: aSQFileStruct
	"Pass a struct SQFile on the stack, flush the external file stream."

	<primitive: 'primitiveSQFileFlush' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 7/22/2000 17:29'!
primSQFileSetBlocking: aSQFileStruct
	"Pass a struct SQFile on the stack, and call fcntl() to set the file non-blocking."

	<primitive: 'primitiveSQFileSetBlocking' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 7/22/2000 17:29'!
primSQFileSetNonBlocking: aSQFileStruct
	"Pass a struct SQFile on the stack, and call fcntl() to set the file non-blocking."

	<primitive: 'primitiveSQFileSetNonBlocking' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 7/22/2000 17:29'!
primSQFileSetUnbuffered: aSQFileStruct
	"Pass a struct SQFile on the stack, set the file non-blocking."

	<primitive: 'primitiveSQFileSetUnbuffered' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 8/7/2005 12:33'!
primSizeOfInt
	"Size of an integer for this C compiler on this machine."

	<primitive: 'primitiveSizeOfInt' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 8/7/2005 12:33'!
primSizeOfPointer
	"Size of a void pointer for this C compiler on this machine."

	<primitive: 'primitiveSizeOfPointer' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 1/31/2020 00:15'!
primSpaceFor: byteSize InDirectoryPath: dirPath

	<primitive: 'primitiveSpaceForByteSizeInDirectoryPath' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/17/2020 16:57'!
primSysconf: optionIndex
	"Answer the result of sysconf() for the supplied option index, and integer
	value typically defined in unistd.h or limits.h"

	<primitive: 'primitiveSysconf' module: 'UnixOSProcessPlugin'>
	^ self primitiveFailed
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 11/28/2010 14:06'!
primTestEndOfFileFlag: aSqFileStruct
	"Answer whether the file represented by aSqFileStruct is at end of file, as determined
	by a call to feof(). This is different from StandardFileStream>>primAtEnd: which answers
	true if the file pointer is at the end of the file, but which does not call feof() to
	determine that an end of file condition has occurred. The difference is significant
	if aSqFileStruct represents a pipe or a device file, which may not be positionable
	in the sense of a conventional disk file."

	<primitive: 'primitiveTestEndOfFileFlag' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 1/4/2004 15:29'!
primTestLockableFileRegion: aSQFileStruct offset: begin length: len exclusive: flag
	"Pass a struct SQFile on the stack, and check for ability to lock the specified region.
	If the exclusive flag is true, then specify an exclusive (F_WRLCK) lock on the
     file. Otherwise, specify a shared (F_RDLCK) lock. Any number of Unix processes
     may hold  a read lock (shared lock) on a file region, but only one process may
     hold a write lock (exclusive lock).

	If length is zero, then the request is for the entire file to be locked, including
	region extents that have not yet been allocated for the file.

	If the fcntl() call fails, answer -1 (the result of the failed call). Otherwise,
	answer an array with the following six fields:
		lockable (true or false)
		l_pid (pid of the process preventing this lock request, or nil)
		l_type (request type F_WRLCK or F_RDLOCK of the process preventing this lock request)
		l_whence (the SEEK_SET, SEEK_CUR, or SEEK_END value of the lock preventing this lock request).
		l_start (offset of the region lock preventing this lock request)
		l_len (length of the region lock preventing this lock request)"

	<primitive: 'primitiveTestLockableFileRegion' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 5/17/2009 21:07'!
primUnixFileClose: integerFileNumber
	"Close a file handle at the close(2) level, using a handle returned by
	#primUnixFileNumber."

	<primitive: 'primitiveUnixFileClose' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 2/11/2001 16:23'!
primUnixFileNumber: aSQFileStruct
	"Pass a struct SQFile on the stack, and answer the corresponding Unix file number."

	<primitive: 'primitiveUnixFileNumber' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 1/3/2004 23:08'!
primUnlockFileRegion: aSQFileStruct offset: begin length: len
	"Pass a struct SQFile on tthe stack, and unlock the specified region.
	Answer the result of the call to fcntl(). If the region is in the file lock cache,
	remove it, but otherwise ignore the cache. The cache supports Win32 semantics
	within a single Squeak image, but not across separate images, therefore the
	unlock should be attempted regardless of whether this image thinks that the
	region has previously been locked. Answer the result of the call to fcntl()."

	<primitive: 'primitiveUnlockFileRegion' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 12/18/2022 13:45'!
primUnsetEnv: aKeyString
	"Remove an environment variable from the external OS process environment."

	^ self primUnsetEnvPrivate: (aKeyString, (Character null) asString) asByteArray! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 12/9/2022 19:32:28'!
primUnsetEnvPrivate: aByteString
	"Dangerous, primitive may not check class type properly, VM crash possible"

	<primitive: 'primitiveUnsetEnv' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 8/30/2003 18:29'!
primXDisplayControlPluginModuleName
	"Answer a string containing the module name string for the display control plugin."

	"OSProcess accessor primXDisplayControlPluginModuleName"

	<primitive: 'primitiveModuleName' module: 'XDisplayControlPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 8/30/2003 18:32'!
primXDisplayControlPluginModuleVersionString
	"Answer a string containing the version string for the display control plugin."

	"OSProcess accessor primXDisplayControlPluginModuleVersionString"

	<primitive: 'primitiveVersionString' module: 'XDisplayControlPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 12/18/2022 11:32'!
fileProtectionMask: aPathString
	"Call stat(2) to obtain the file protection mask for a file."

	"OSProcess accessor fileProtectionMask: '/bin/sh'"
	"OSProcess accessor fileProtectionMask: '/etc/hosts'"
	"OSProcess accessor fileProtectionMask: '/bin/su'"
	"OSProcess accessor fileProtectionMask: '/bin/NOSUCHFILE'"
	"OSProcess accessor fileProtectionMask: 12345"

	| mask |
	mask _ self primFileProtectionMask: aPathString asByteArray.
	(mask == nil) ifTrue: [^ nil].
	(mask isKindOf: Integer)
		ifTrue: [^ self primErrorMessageAt: mask].
	^ mask
! !

!UnixOSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 12/18/2022 11:33'!
fileStat: aPathString
	"Call stat(2) to obtain the UID, GID, and file protection mask for a file."

	"OSProcess accessor fileStat: '/var/spool/news'"
	"OSProcess accessor fileStat: '/etc/hosts'"
	"OSProcess accessor fileStat: '/bin/su'"
	"OSProcess accessor fileStat: '/bin/NOSUCHFILE'"
	"OSProcess accessor fileStat: 12345"

	| mask |
	mask _ self primFileStat: aPathString asByteArray.
	(mask == nil) ifTrue: [^ nil].
	(mask isKindOf: Integer)
		ifTrue: [^ self primErrorMessageAt: mask].
	^ mask
! !

!UnixOSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 10/6/2001 12:08'!
isExecutable: aPathName
	"Answer true if file at aPathName has execute permission for this process."

	"OSProcess accessor isExecutable: '/bin/sh'"
	"OSProcess accessor isExecutable: '/no/such/file'"

	^ self isExecutable: aPathName forUser: self primGetUid inGroup: self primGetGid

! !

!UnixOSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 5/1/2001 22:53'!
isExecutable: aPathName forUser: uid inGroup: gid
	"Answer true if file at aPathName has execute permission for a user
	identified by user uid and group gid."

	"OSProcess accessor
		isExecutable: '/bin/sh'
		forUser: OSProcess accessor primGetUid
		inGroup: OSProcess accessor primGetGid"

	| fStat suid sgid user group protectionMask |
	fStat _ self fileStat: aPathName.
	(fStat isKindOf: String)
		ifTrue: [self inform: aPathName, ': ', fStat. ^ nil].
	protectionMask _ fStat at: 3.
	((protectionMask at: 4) allMask: 1)
		ifTrue: [^ true]. "Test executable by any user ID"
	sgid _ (protectionMask at: 1) allMask: 2.
	sgid
		ifTrue: [group _ gid]
		ifFalse: [group _ fStat at: 2].
	((gid == group) and: [(protectionMask at: 3) allMask: 1])
		ifTrue: [^ true]. "Test executable by my group ID"
	suid _ (protectionMask at: 1) allMask: 4.
	suid
		ifTrue: [user _ uid]
		ifFalse: [user _ fStat at: 1].
	((uid == user) and: [(protectionMask at: 2) allMask: 1])
		ifTrue: [^ true]. "Test executable by my user ID"
	^ false

! !

!UnixOSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 5/1/2001 23:09'!
isReadable: aPathName
	"Answer true if file at aPathName has read permission for this process."

	"OSProcess accessor isWritable: '/bin/sh'"

	^ self isReadable: aPathName forUser: self primGetUid inGroup: self primGetGid

! !

!UnixOSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 5/1/2001 23:00'!
isReadable: aPathName forUser: uid inGroup: gid
	"Answer true if file at aPathName has read permission for a user
	identified by user uid and group gid."

	"OSProcess accessor
		isReadable: '/bin/sh'
		forUser: OSProcess accessor primGetUid
		inGroup: OSProcess accessor primGetGid"

	| fStat user group protectionMask |
	fStat _ self fileStat: aPathName.
	(fStat isKindOf: String)
		ifTrue: [self inform: aPathName, ': ', fStat. ^ nil].
	protectionMask _ fStat at: 3.
	((protectionMask at: 4) allMask: 4)
		ifTrue: [^ true]. "Test readable by any user ID"
	group _ fStat at: 2.
	((gid == group) and: [(protectionMask at: 3) allMask: 4])
		ifTrue: [^ true]. "Test readable by my group ID"
	user _ fStat at: 1.
	((uid == user) and: [(protectionMask at: 2) allMask: 4])
		ifTrue: [^ true]. "Test readable by my user ID"
	^ false

! !

!UnixOSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 5/1/2001 23:09'!
isWritable: aPathName
	"Answer true if file at aPathName has write permission for this process."

	"OSProcess accessor isWritable: '/bin/sh'"

	^ self isWritable: aPathName forUser: self primGetUid inGroup: self primGetGid

! !

!UnixOSProcessAccessor methodsFor: 'file testing' stamp: 'dtl 5/1/2001 23:08'!
isWritable: aPathName forUser: uid inGroup: gid
	"Answer true if file at aPathName has read permission for a user
	identified by user uid and group gid."

	"OSProcess accessor
		isWritable: '/bin/sh'
		forUser: OSProcess accessor primGetUid
		inGroup: OSProcess accessor primGetGid"

	| fStat user group protectionMask |
	fStat _ self fileStat: aPathName.
	(fStat isKindOf: String)
		ifTrue: [self inform: aPathName, ': ', fStat. ^ nil].
	protectionMask _ fStat at: 3.
	((protectionMask at: 4) allMask: 2)
		ifTrue: [^ true]. "Test writable by any user ID"
	group _ fStat at: 2.
	((gid == group) and: [(protectionMask at: 3) allMask: 2])
		ifTrue: [^ true]. "Test writable by my group ID"
	user _ fStat at: 1.
	((uid == user) and: [(protectionMask at: 2) allMask: 2])
		ifTrue: [^ true]. "Test writable by my user ID"
	^ false

! !

!UnixOSProcessAccessor methodsFor: 'initialize - release' stamp: 'dtl 7/6/2010 21:22'!
finalize
	"Clean up grimReaper and associated semaphore."

	self grimReaper ifNotNil:
			[grimReaper terminate.
			grimReaper _ nil].
	self sigChldSemaphore ifNotNil:
			[self restoreSigChld.
			self sigChldSemaphore: nil]
! !

!UnixOSProcessAccessor methodsFor: 'initialize - release' stamp: 'eem 11/19/2021 12:34'!
grimReaperProcess
	"This is a process which waits for the death of a child OSProcess, and 
	informs any dependents of the change. Use SIGCHLD events if possible,
	otherwise a Delay to poll for exiting child processes."

	| eventWaiter processSynchronizationDelay |
	^ self canAccessSystem
		ifTrue: [ eventWaiter _ (self canAccessSystem
				and: [ self canForwardExternalSignals ])
				ifTrue: [ self sigChldSemaphore	"semaphore signaled by SIGCHLD" ]
				ifFalse: [ Delay forMilliseconds: 200	"simple polling loop" ].
			processSynchronizationDelay _ Delay forMilliseconds: 20.
			grimReaper
				ifNil: [ grimReaper _ [ [ (eventWaiter respondsTo: #waitTimeoutMSecs:)
						ifTrue: [ eventWaiter waitTimeoutMSecs: 1000	"semaphore with timeout" ]
						ifFalse: [ eventWaiter wait ].
					processSynchronizationDelay wait.	"Avoids lost signals in heavy process switching"
					self changed: #childProcessStatus ] repeat ] newProcess.
					grimReaper priority: Processor userSchedulingPriority.
					grimReaper resume.
					"name selected to look reasonable in the process browser"
					grimReaper
						name:
							((ReadStream on: grimReaper hash asString) next: 5)
								, ': the child OSProcess watcher' ] ]
		ifFalse: [ grimReaper ifNotNil: [grimReaper priority: Processor userSchedulingPriority ]]! !

!UnixOSProcessAccessor methodsFor: 'initialize - release' stamp: 'dtl 7/7/2010 07:46'!
initialize
	"Call this method when an instance is first created, or to refresh after an image
	restart to clean up from the previous session. Notify dependents of my singleton
	instance if the image has restarted in a different OS process (this is not the case
	when #startUp is called after a simple image save)."

	self finalize.
	super initialize.
	ThisProcessPid ~~ self primGetPid
		ifTrue: 
			["Image has been restarted and is now running in a different OS process"
			ThisProcessPid _ self primGetPid.
			self changed: #startUp].
	self changed.
	self restartChildWatcherProcess.

! !

!UnixOSProcessAccessor methodsFor: 'initialize - release' stamp: 'dtl 8/15/2010 15:30'!
restartChildWatcherProcess
	"OSProcess accessor restartChildWatcherProcess"

	self finalize.
	^ self grimReaperProcess
! !

!UnixOSProcessAccessor methodsFor: 'fork and exec' stamp: 'dtl 1/25/2004 21:43'!
forkAndExec: executableFile
	stdIn: inputFileHandle
	stdOut: outputFileHandle
	stdErr: errorFileHandle
	argBuf: argVec
	argOffsets: argOffsets
	envBuf: envVec
	envOffsets: envOffsets
	workingDir: pathString

	"Parameters are expected to have been properly prepared by the caller, including string
	values which are to be null terminated strings. In other words, all strings should have
	(Character null) as the last element in the string.

	Parameters should be:
		executableFile: a string with the name of a file to execute.
		stdIn: a fileID ByteArray (struct SQFile in C) to be used as standard input.
		stdOut: a fileID ByteArray to be used as standard output.
		stdErr: a fileID ByteArray to be used as standard error.
		argVec: a String arranged to look more or less like a char **, but with the addresses not yet fixed.
		argOffsets: an Array of offsets for fixing up the argVec addresses.
		envVec: a String arranged to look more or less like a char **, but with the addresses not yet fixed.
		envOffsets: an Array of offsets for fixing up the envVec addresses.
		workingDir: a null terminated path name String, or nil.
	The envVec parameter may be nil, in which case envOffsets is ignored. workingDir may be nil. The
	other parameters are required. Parameters with nil value indicate that current values for this process
	should be used."

	^ self primForkExec: executableFile
			stdIn: inputFileHandle
			stdOut: outputFileHandle
			stdErr: errorFileHandle
			argBuf: argVec
			argOffsets: argOffsets
			envBuf: envVec
			envOffsets: envOffsets
			workingDir: pathString
! !

!UnixOSProcessAccessor methodsFor: 'fork and exec' stamp: 'dtl 4/27/2011 21:01'!
forkSqueak
	"Clone this Squeak Smalltalk image in a child OSProcess. The child is the same as the
	parent, except for its new X session connection, and the return value of this method,
	which is zero for the child process, and a positive integer equal to the pid of the child
	for the parent process.

	The child should not depend on using existing connections to external resources. For
	example, the child may lose its connections to stdin, stdout, and stderr after its parent
	exits."

	self safeToForkSqueak
		ifTrue: [^self primForkSqueak].
	[self notify: 'forkSqueak requires XDisplayControlPlugin']
		ensure: [^nil]
! !

!UnixOSProcessAccessor methodsFor: 'fork and exec' stamp: 'dtl 10/8/2005 09:55'!
nice: inc
	"Change the scheduling priority of this OS process by the given nice increment.
	A positive increment decreases the priority. Only the superuser can specify
	a negative value to increase the priority. A typical use is to increase the
	nice value by 1 in order to make the Squeak VM run at lower priority. This
	may be useful for a background Squeak doing an image save or other non-
	interactive process."

	"OSProcess accessor nice: 1"

	^ self primNice: inc
! !

!UnixOSProcessAccessor methodsFor: 'fork and exec' stamp: 'dtl 11/28/2010 12:35'!
primForkExec: executableFile
	stdIn: inputFileHandle
	stdOut: outputFileHandle
	stdErr: errorFileHandle
	argBuf: argVec
	argOffsets: argOffsets
	envBuf: envVec
	envOffsets: envOffsets
	workingDir: pathString

	"Parameters are expected to have been properly prepared by the caller, including string
	values which are to be null terminated strings. In other words, all strings should have
	(Character null) as the last element in the string.

	Parameters should be:
		executableFile: a string with the name of a file to execute.
		stdIn: a fileID ByteArray (struct SQFile in C) to be used as standard input.
		stdOut: a fileID ByteArray to be used as standard output.
		stdErr: a fileID ByteArray to be used as standard error.
		argVec: a String arranged to look more or less like a char **, but with the addresses not yet fixed.
		argOffsets: an Array of offsets for fixing up the argVec addresses.
		envVec: a String arranged to look more or less like a char **, but with the addresses not yet fixed.
		envOffsets: an Array of offsets for fixing up the envVec addresses.
		workingDir: a null terminated path name String, or nil.
	The envVec parameter may be nil, in which case envOffsets is ignored. workingDir may be nil. The
	other parameters are required. Parameters with nil value indicate that current values for this process
	should be used."

	<primitive: 'primitiveForkExec' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'fork and exec' stamp: 'dtl 11/28/2010 12:37'!
primForkSqueak
	"Clone this Squeak Smalltalk image in a child OSProcess. The child is the same as the
	parent, except for its new X session connection, and the return value of this method,
	which is zero for the child process, and a positive integer equal to the pid of the child
	for the parent process.

	The child should not depend on using existing connections to external resources. For
	example, the child may lose its connections to stdin, stdout, and stderr after its parent
	exits."

	<primitive: 'primitiveForkSqueakWithoutSigHandler' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'fork and exec' stamp: 'dtl 10/14/2001 11:15'!
primGetChildExitStatus: childPid
	"Clean up after the death of a child process, and answer the exit status of the child process."

	<primitive: 'primitiveReapChildProcess' module: 'UnixOSProcessPlugin'>
	^ Array with: childPid with: nil! !

!UnixOSProcessAccessor methodsFor: 'fork and exec' stamp: 'dtl 10/8/2005 09:55'!
primNice: inc
	"Change the scheduling priority of this OS process by the given nice increment."

	<primitive: 'primitiveNice' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'fork and exec' stamp: 'dtl 8/18/2014 20:16'!
safeToForkSqueak
	"The forkSqueak method is currently supported only for X11 displays with
	XDisplayControlPlugin available. If the plugin is not available, the child Squeak
	VM will continue interacting with the X display, which leads to errors in the
	parent Squeak, including VM crashes."

	^self canControlXDisplay
		or: [ ThisOSProcess isHeadless ]! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:51'!
forwardSigChld
	"Set a signal handler for SIGCHLD. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	"OSProcess accessor forwardSigChld"

	^ self forwardSignal: self primSigChldNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:51'!
forwardSigHup
	"Set a signal handler for SIGHUP. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	"OSProcess accessor forwardSigHup"

	^ self forwardSignal: self primSigHupNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:51'!
forwardSigInt
	"Set a signal handler for SIGINT. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	"OSProcess accessor forwardSigInt"

	^ self forwardSignal: self primSigIntNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:51'!
forwardSigKill
	"Set a signal handler for SIGKILL. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	"OSProcess accessor forwardSigKill"

	self notify: 'SIGKILL and SIGSTOP signals cannot be caught, see man signal(2)'.
	^ self forwardSignal: self primSigKillNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:52'!
forwardSigPipe
	"Set a signal handler for SIGPIPE. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	"OSProcess accessor forwardSigPipe"

	^ self forwardSignal: self primSigPipeNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:52'!
forwardSigQuit
	"Set a signal handler for SIGQUIT. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	"OSProcess accessor forwardSigQuit"

	^ self forwardSignal: self primSigQuitNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:52'!
forwardSigTerm
	"Set a signal handler for SIGTERM. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	"OSProcess accessor forwardSigTerm"

	^ self forwardSignal: self primSigTermNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:52'!
forwardSigUsr1
	"Set a signal handler for SIGUSR1. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	"OSProcess accessor forwardSigUsr1"

	^ self forwardSignal: self primSigUsr1Number
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:52'!
forwardSigUsr2
	"Set a signal handler for SIGUSR2. Answer a new Semaphore, or nil if unable
	to set the handler (possibly because it has already been set)."

	"OSProcess accessor forwardSigUsr1"

	^ self forwardSignal: self primSigUsr2Number
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 12/6/2002 18:28'!
forwardSignal: signalNumber 
	"Set a signal handler in the VM which will signal a Smalltalk semaphore 
	at semaphoreIndex whenever an external signal sigNum is received.
	Answer a new Semaphore, or nil if unable to set the handler (possibly
	because it has already been set). A Smalltalk process can wait on the
	Semaphore, and take action when a signal is detected. See man(7) signal
	for signal number definitions on your unix system."

	"OSProcess accessor forwardSignal: OSProcess accessor primSigIntNumber"

	| sema index |
	sema _ Semaphore new.
	index _ Smalltalk registerExternalObject: sema.
	(self primForwardSignal: signalNumber toSemaphore: index)
		ifNil:
			[Smalltalk unregisterExternalObject: sema.
			^ nil].
	^ sema! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:45'!
restoreSigChld
	"Unset a SIGCHLD signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	"OSProcess accessor restoreSigChld"

	^ self restoreSignal: self primSigChldNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:47'!
restoreSigHup
	"Unset a SIGHUP signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	"OSProcess accessor restoreSigHup"

	^ self restoreSignal: self primSigHupNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:46'!
restoreSigInt
	"Unset a SIGINT signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	"OSProcess accessor restoreSigInt"

	^ self restoreSignal: self primSigIntNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:48'!
restoreSigKill
	"Unset a SIGKILL signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	"OSProcess accessor restoreSigKill"

	self notify: 'SIGKILL and SIGSTOP signals cannot be caught, see man signal(2)'.
	^ self restoreSignal: self primSigIntNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:48'!
restoreSigPipe
	"Unset a SIGPIPE signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	"OSProcess accessor restoreSigPipe"

	^ self restoreSignal: self primSigPipeNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:49'!
restoreSigQuit
	"Unset a SIGQUIT signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	"OSProcess accessor restoreSigQuit"

	^ self restoreSignal: self primSigQuitNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:49'!
restoreSigTerm
	"Unset a SIGTERM signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	"OSProcess accessor restoreSigTerm"

	^ self restoreSignal: self primSigTermNumber
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:49'!
restoreSigUsr1
	"Unset a SIGUSR1 signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	"OSProcess accessor restoreSigUsr1"

	^ self restoreSignal: self primSigUsr1Number
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 11/4/2005 06:50'!
restoreSigUsr2
	"Unset a SIGUSR2 signal handler and unregister the Smalltalk semaphore.
	Answer the unregistered Semaphore, or nil if unable to restore the signal
	(possibly because no handler had been set)."

	"OSProcess accessor restoreSigUsr2"

	^ self restoreSignal: self primSigUsr2Number
! !

!UnixOSProcessAccessor methodsFor: 'signal forwarding' stamp: 'dtl 12/28/2002 15:33'!
restoreSignal: signalNumber 
	"Unset a signal handler and unregister the Smalltalk semaphore. Answer
	the unregistered Semaphore, or nil if unable to restore the signal (possibly
	because no handler had been set)."

	"OSProcess accessor restoreSignal: OSProcess accessor primSigIntNumber"

	| semaphoreIndex sema |
	semaphoreIndex _ self primSemaIndexFor: signalNumber.
	semaphoreIndex ifNotNil:
		[sema _ Smalltalk externalObjects at: semaphoreIndex ifAbsent: [].
		sema ifNotNil:
			[self primForwardSignal: signalNumber toSemaphore: nil.
			Smalltalk unregisterExternalObject: sema]].
	^ sema
! !

!UnixOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 19:38'!
getStdErrHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard error for the
	OS process in which I am currently executing."

	^ self primGetStdErrHandle! !

!UnixOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 19:39'!
getStdInHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard input for the
	OS process in which I am currently executing."

	^ self primGetStdInHandle! !

!UnixOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 19:39'!
getStdOutHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard output for the
	OS process in which I am currently executing."

	^ self primGetStdOutHandle! !

!UnixOSProcessAccessor methodsFor: 'pthreads' stamp: 'dtl 3/17/2007 23:14'!
getThreadID
	"Answer the ID of the pthread that is currently executing (the interpreter thread).
	A thread ID may be a 64 bit value on some platforms, so answer a byte array
	containing the value in machine-dependent byte order."

	"OSProcess accessor getThreadID"

	| osppVer |
	[osppVer _ OSProcess accessor osppModuleVersionString asNumber]
		on: Error
		do: [^ nil].
	(osppVer notNil and: [osppVer >= 4.2])
		ifTrue: [^ self primGetThreadID]
		ifFalse: [^ nil]
! !

!UnixOSProcessAccessor methodsFor: 'pthreads' stamp: 'dtl 3/5/2006 10:51'!
primGetThreadID
	"Answer the ID of the pthread that is currently executing (the interpreter thread).
	A thread ID may be a 64 bit value on some platforms, so answer a byte array
	containing the value in machine-dependent byte order."

	"OSProcess accessor primGetThreadID"

	<primitive: 'primitiveGetThreadID' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 2/14/2004 15:10'!
isLockableFile: aFileStream
	"Check for ability to place an exclusive lock on the entire file represented by
	aFileStream. An exclusive lock (write lock) permits only one OS process to hold
	the lock. Answer true if the region is lockable."

	^ self isLockableFile: aFileStream exclusive: true
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'jf 2/22/2004 17:30'!
isLockableFile: aFileStream exclusive: flag
	"Check for ability to place an exclusive lock on the entire file represented by
	aFileStream. An exclusive lock (write lock) permits only one OS process to hold
	the lock. Answer true if the region is lockable."

	| fileLock |
	aFileStream ifNil: [^ false].
	fileLock _ OSFileLock onFile: aFileStream exclusive: flag.
	^ self isLockableFileRegion: fileLock
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 2/14/2004 15:09'!
isLockableFile: aFileStream from: start to: end
	"Pass a struct SQFile on the stack, and check for ability to lock the specified region.
	If the exclusive flag is true, then specify an exclusive (F_WRLCK) lock on the
     file. Otherwise, specify a shared (F_RDLCK) lock. Any number of Unix processes
     may hold  a read lock (shared lock) on a file region, but only one process may
     hold a write lock (exclusive lock).

	If length is zero, then the request is for the entire file to be locked, including
	region extents that have not yet been allocated for the file.

	Answer true if the region is lockable."

	^ self isLockableFile: aFileStream from: start to: end exclusive: true
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 2/22/2004 15:10'!
isLockableFile: aFileStream from: start to: end exclusive: flag
	"Pass a struct SQFile on the stack, and check for ability to lock the specified region.
	If the exclusive flag is true, then specify an exclusive (F_WRLCK) lock on the
     file. Otherwise, specify a shared (F_RDLCK) lock. Any number of Unix processes
     may hold  a read lock (shared lock) on a file region, but only one process may
     hold a write lock (exclusive lock).

	If length is zero, then the request is for the entire file to be locked, including
	region extents that have not yet been allocated for the file.

	Answer true if the region is lockable."

	| fileRegion |
	aFileStream ifNil: [^ false].
	fileRegion _ OSFileRegionLock onFile: aFileStream from: start to: end exclusive: flag.
	^ self isLockableFileRegion: fileRegion
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'jf 2/22/2004 18:42'!
isLockableFileRegion: aFileLock
	"Pass a struct SQFile on the stack, and check for ability to lock the specified region.
	If the exclusive flag is true, then specify an exclusive (F_WRLCK) lock on the
     file. Otherwise, specify a shared (F_RDLCK) lock. Any number of Unix processes
     may hold  a read lock (shared lock) on a file region, but only one process may
     hold a write lock (exclusive lock).

	If length is zero, then the request is for the entire file to be locked, including
	region extents that have not yet been allocated for the file.

	Answer true if the region is lockable."

	aFileLock ifNil: [^ false].
	(self canAcquireLock: aFileLock) ifFalse: [^ false].
	
	^ aFileLock test
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 2/14/2004 15:01'!
lockFile: aFileStream
	"Request an exclusive lock on the entire file represented by aFileStream. The
	exclusive lock (write lock) permits only one OS process to hold the lock. Answer
	a descriptor for the locked file region, an Array of file handle and region interval;
	or answer nil on error.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	^ self lockFile: aFileStream exclusive: true
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 5/11/2006 23:40'!
lockFile: aFileStream exclusive: flag
	"Request an exclusive lock on the entire file represented by aFileStream. The
	exclusive lock (write lock) permits only one OS process to hold the lock. Answer
	a descriptor for the locked file region, an Array of file handle and region interval;
	or answer nil on error.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	^ self lockFile: aFileStream exclusive: flag ifFail: nil
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 5/11/2006 23:39'!
lockFile: aFileStream exclusive: flag ifFail: failBlock
	"Request an exclusive lock on the entire file represented by aFileStream. The
	exclusive lock (write lock) permits only one OS process to hold the lock. Answer
	a descriptor for the locked file region, an Array of file handle and region interval;
	or answer nil on error.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	| fileLock |
	aFileStream ifNil: [^ failBlock value].
	fileLock _ OSFileLock onFile: aFileStream exclusive: flag.
	^ self lockFileRegion: fileLock ifFail: failBlock
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 2/14/2004 15:01'!
lockFile: aFileStream from: start to: end
	"Pass a struct SQFile on the stack, and request a lock on the specified region.
	If the exclusive flag is true, then request an exclusive (F_WRLCK) lock on the
     file. Otherwise, request a shared (F_RDLCK) lock. Any number of Unix processes
     may hold a read lock (shared lock) on a file region, but only one process may
     hold a write lock (exclusive lock). Answer a descriptor for the locked file region,
	an Array of file handle and region interval; or answer nil on error.

	If length is zero, then the request is for the entire file to be locked, including
	region extents that have not yet been allocated for the file.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	^ self lockFile: aFileStream from: start to: end exclusive: true
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 2/22/2004 15:10'!
lockFile: aFileStream from: start to: end exclusive: flag
	"Pass a struct SQFile on the stack, and request a lock on the specified region.
	If the exclusive flag is true, then request an exclusive (F_WRLCK) lock on the
     file. Otherwise, request a shared (F_RDLCK) lock. Any number of Unix processes
     may hold a read lock (shared lock) on a file region, but only one process may
     hold a write lock (exclusive lock). Answer a descriptor for the locked file region,
	an Array of file handle and region interval; or answer nil on error.

	If length is zero, then the request is for the entire file to be locked, including
	region extents that have not yet been allocated for the file.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	| fileRegion |
	aFileStream ifNil: [^ nil].
	fileRegion _ OSFileRegionLock onFile: aFileStream from: start to: end exclusive: flag.
	^ self lockFileRegion: fileRegion
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 8/30/2009 14:51'!
lockFile: fileStream from: start to: end exclusive: writeLockFlag ifFail: failBlock

	| lock |
	lock _ OSFileRegionLock onFile: fileStream from: start to: end exclusive: writeLockFlag.
	^ self lockFileRegion: lock ifFail: [failBlock value]! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 11/28/2010 19:07'!
lockFile: fileStream from: start to: end ifFail: failBlock

	^ self lockFile: fileStream from: start to: end exclusive: true ifFail: failBlock
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 5/11/2006 23:41'!
lockFile: aFileStream ifFail: failBlock
	"Request an exclusive lock on the entire file represented by aFileStream. The
	exclusive lock (write lock) permits only one OS process to hold the lock. Answer
	a descriptor for the locked file region, an Array of file handle and region interval;
	or answer nil on error.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	^ self lockFile: aFileStream exclusive: true ifFail: failBlock
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 5/8/2006 07:04'!
lockFileRegion: aFileLock
	"Pass a struct SQFile on the stack, and request a lock on the specified region.
	If the exclusive flag is true, then request an exclusive (F_WRLCK) lock on the
     file. Otherwise, request a shared (F_RDLCK) lock. Any number of Unix processes
     may hold a read lock (shared lock) on a file region, but only one process may
     hold a write lock (exclusive lock). Answer a descriptor for the locked file region,
	an Array of file handle and region interval; or answer nil on error.

	If length is zero, then the request is for the entire file to be locked, including
	region extents that have not yet been allocated for the file.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	^ self lockFileRegion: aFileLock ifFail: [nil]
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 5/8/2006 07:03'!
lockFileRegion: aFileLock ifFail: failBlock
	"Pass a struct SQFile on the stack, and request a lock on the specified region.
	If the exclusive flag is true, then request an exclusive (F_WRLCK) lock on the
     file. Otherwise, request a shared (F_RDLCK) lock. Any number of Unix processes
     may hold a read lock (shared lock) on a file region, but only one process may
     hold a write lock (exclusive lock). Answer a descriptor for the locked file region,
	an Array of file handle and region interval; or answer nil on error.

	If length is zero, then the request is for the entire file to be locked, including
	region extents that have not yet been allocated for the file.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	aFileLock ifNil: [^ failBlock value].
	"Check region lock overlap for Win32 compatibility"
	(self canAcquireLock: aFileLock) ifFalse: [^ failBlock value].
	
	aFileLock lock
		ifFalse: [^ failBlock value]
		ifTrue:
			[self register: aFileLock.
			^ aFileLock]
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 4/10/2005 15:17'!
unlockAllForFile: aFileStream
	"Unlock and uncache all locks associated with aFileStream.  This could be called
	before closing a stream, for example."

	(self registeredLocksForFile: aFileStream) do: [:ea | self unlockFileRegion: ea]
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 2/22/2004 15:02'!
unlockFile: aFileStream
	"Unlock the file represented by aFileStream.  Answer a descriptor for the unlocked
	file region, an Array of file handle and region interval; or answer nil on error
	or if the region did not appear in the cache.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	^ self unlockFile: aFileStream exclusive: true
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'jf 2/22/2004 17:30'!
unlockFile: aFileStream exclusive: flag
	"Unlock the file represented by aFileStream.  Answer a descriptor for the unlocked
	file region, an Array of file handle and region interval; or answer nil on error
	or if the region did not appear in the cache.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	| fileLock |
	aFileStream ifNil: [^ nil].
	fileLock _ OSFileLock onFile: aFileStream exclusive: flag.
	^ self unlockFileRegion: fileLock
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 5/12/2006 07:05'!
unlockFile: aFileStream exclusive: flag ifFail: failBlock
	"Unlock the file represented by aFileStream.  Answer a descriptor for the unlocked
	file region, an Array of file handle and region interval; or answer nil on error
	or if the region did not appear in the cache.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	| fileLock |
	aFileStream ifNil: [^ failBlock value].
	fileLock _ OSFileLock onFile: aFileStream exclusive: flag.
	^ self unlockFileRegion: fileLock ifFail: failBlock

! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 2/22/2004 15:01'!
unlockFile: aFileStream from: start to: end
	"Pass a struct SQFile on the stack, and unlock the specified region.
	Answer the result of the call to fcntl(). If the region is in the file lock cache,
	remove it, but otherwise ignore the cache. The cache supports Win32 semantics
	within a single Squeak image, but not across separate images, therefore the
	unlock should be attempted regardless of whether this image thinks that the
	region has previously been locked. Answer a descriptor for the unlocked file
	region, an Array of file handle and region interval; or answer nil on error
	or if the region did not appear in the cache.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	^ self unlockFile: aFileStream from: start to: end exclusive: true
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 2/22/2004 15:09'!
unlockFile: aFileStream from: start to: end exclusive: flag
	"Pass a struct SQFile on the stack, and unlock the specified region.
	Answer the result of the call to fcntl(). If the region is in the file lock cache,
	remove it, but otherwise ignore the cache. The cache supports Win32 semantics
	within a single Squeak image, but not across separate images, therefore the
	unlock should be attempted regardless of whether this image thinks that the
	region has previously been locked. Answer a descriptor for the unlocked file
	region, an Array of file handle and region interval; or answer nil on error
	or if the region did not appear in the cache.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	| fileRegion |
	aFileStream ifNil: [^ nil].
	fileRegion _ OSFileRegionLock onFile: aFileStream from: start to: end exclusive: flag.
	^ self unlockFileRegion: fileRegion
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 8/30/2009 14:54'!
unlockFile: fileStream from: start to: end exclusive: writeLockFlag ifFail: failBlock

	| lock |
	lock _ OSFileRegionLock onFile: fileStream from: start to: end exclusive: writeLockFlag.
	^ self unlockFileRegion: lock ifFail: [failBlock value]! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 8/30/2009 16:25'!
unlockFile: fileStream from: start to: end ifFail: failBlock

	^ self unlockFile: fileStream from: start to: end exclusive: true ifFail: failBlock
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 5/12/2006 07:06'!
unlockFile: aFileStream ifFail: failBlock
	"Unlock the file represented by aFileStream.  Answer a descriptor for the unlocked
	file region, an Array of file handle and region interval; or answer nil on error
	or if the region did not appear in the cache.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	^ self unlockFile: aFileStream exclusive: true ifFail: failBlock
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 5/8/2006 07:12'!
unlockFileRegion: aFileLock
	"Pass a struct SQFile on the stack, and unlock the specified region.
	Answer the result of the call to fcntl(). If the region is in the file lock cache,
	remove it, but otherwise ignore the cache. The cache supports Win32 semantics
	within a single Squeak image, but not across separate images, therefore the
	unlock should be attempted regardless of whether this image thinks that the
	region has previously been locked. Answer a descriptor for the unlocked file
	region, an Array of file handle and region interval; or answer nil on error
	or if the region did not appear in the cache.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	^ self unlockFileRegion: aFileLock ifFail: [nil]
! !

!UnixOSProcessAccessor methodsFor: 'file locking' stamp: 'dtl 5/20/2006 12:46'!
unlockFileRegion: aFileLock ifFail: failBlock
	"Pass a struct SQFile on the stack, and unlock the specified region.
	Answer the result of the call to fcntl(). If the region is in the file lock cache,
	remove it, but otherwise ignore the cache. The cache supports Win32 semantics
	within a single Squeak image, but not across separate images, therefore the
	unlock should be attempted regardless of whether this image thinks that the
	region has previously been locked. Answer a descriptor for the unlocked file
	region, an Array of file handle and region interval; or answer nil on error
	or if the region did not appear in the cache.

	Warning: The registry permits compatibility with Win32 file locking semantics,
	but only within a single Squeak image. Multiple cooperating images must not
	rely on the overlap checking, because the registry is local to this image and
	cannot be shared across images in different OS process contexts."

	aFileLock ifNil: [^ failBlock value].
	"Check region lock overlap for Win32 compatibility"
	self emulateWin32FileLocking
		ifTrue: [ | unregisteredLock |
				(unregisteredLock _ self unregister: aFileLock)
					ifNil: [^ failBlock value]
					ifNotNil: [unregisteredLock unlock
						ifTrue: [^ unregisteredLock]
						ifFalse: [^ failBlock value]]]
		ifFalse: [aFileLock unlock
					ifTrue: [^ aFileLock]
					ifFalse: [^ failBlock value]]
! !

!UnixOSProcessAccessor methodsFor: 'VM atexit' stamp: 'dtl 3/17/2007 18:39'!
kill: listOfPids withSignal: signumOrNil
	"On exit of the VM process, send signal to the external processes
	identified by listOfPids. If signumOrNil is nil, the default SIGTERM
	signal will be sent to listOfPids."

	^ self primKill: listOfPids withSignal: signumOrNil
! !

!UnixOSProcessAccessor methodsFor: 'VM atexit' stamp: 'dtl 3/18/2007 10:39'!
killOnVmExit: proxies withSignal: signumOrNil
	"When Squeak uses OSProcess to start a long running external process, it
	may be useful to guarantee that one or more child processes is killed when
	the Squeak VM exits, regardless of whether the normal Squeak shutdown
	processing has occurred. This method arranges for a collection of external
	process proxies to receive a signal when the Squeak VM exits. If signumOrNil
	is nil, the default SIGTERM will be used. Each invocation of this method will
	override the effects of previous calls."

	"OSProcess accessor killOnVmExit: OSProcess thisOSProcess allMyChildren withSignal: nil"

	| pids |
	pids _ (proxies select: [:p | p isRunning] thenCollect: [:e | e pid]) asArray.
	^ self kill: pids withSignal: signumOrNil
! !

!UnixOSProcessAccessor methodsFor: 'pipe open' stamp: 'dtl 8/19/2022 19:34'!
makePipeHandles
	"Create a pipe, and answer an array of two IO accessors for the pipe 
	reader and writer. Set a signal handler to cause SIGPIPE signals to be
	ignored. This will register a Semaphore to receive the SIGPIPE events,
	but we will simply ignore the semaphore, effectively ignoring the
	external OS signals. This is done instead of explicitly telling the OS to
	ignore the signals because it allows the use of a uniform signal
	forwarding mechanism in Squeak, even for signals that are ultimately
	ignored."

	self forwardSigPipe.
	^ self primCreatePipe
! !

!UnixOSProcessAccessor methodsFor: 'temp files and directories' stamp: 'dtl 6/3/2013 22:05'!
mkdtemp: templateString
	"Call mkdtemp() to generate a uniquely named temporary directory from a template
	string. The last six or more characters of template must be XXXXXX and these are
	replaced with characters that make the directory name unique. The directory is then
	created with permissions 0700. Answer the new directory name."

	"OSProcess accessor mkdtemp: 'foodir.XXXXXX' "

	^self primMkdtemp: templateString
! !

!UnixOSProcessAccessor methodsFor: 'temp files and directories' stamp: 'dtl 6/3/2013 22:36'!
mkstemp: templateString

	"Call mkstemp() to generate a unique temporary file from a template string.
	The last six or more characters of template must be XXXXXX and these are
	replaced with a string that make the filename unique. Contents of the template
	string are modified. The file is created with permissions 0600, that is, read plus
	write for owner only. Answer an opened file stream on the newly created file."

	"OSProcess accessor mkstemp: 'foo.XXXXXX' "

	" | s | { s _ 'foo.XXXXXX' . OSProcess accessor mkstemp: s } "

	| fileName sqFile |
	fileName _ templateString copy.
	sqFile _ self primMkstemp: fileName.
	^ AttachableFileStream
		name: fileName
		attachTo: sqFile
		writable: true
! !

!UnixOSProcessAccessor methodsFor: 'temp files and directories' stamp: 'dtl 2/8/2020 15:18'!
mktemp: templateString
	"Call mktemp() to generate a unique temporary filename from a template string.
	No longer supported in OSProcess because mktemp() is considered a security risk.
	Use primitiveMkstemp to avoid file creation race condition, see man 3 mktemp."

	"OSProcess accessor mktemp: 'foo.XXXXXX' "
	"OSProcess accessor mktemp: '/tmp/foo.XXXXXX' "

	^ self error: 'mktemp: support has been removed for security reasons, use mkstemp or mkdtemp: instead'
! !

!UnixOSProcessAccessor methodsFor: 'display handling - old plugin compatibility' stamp: 'dtl 8/3/2003 20:34'!
oldPrimCanConnectToXDisplay: xDisplayName

	"Deprecated - install the XDisplayControlPlugin to eliminate the need to call this method"

	"Open and close a connection to displayName. It the connection was successfully
	opened, answer true; otherwise false. This is intended to check for the ability
	to open an X display prior to actually making the attempt."

	"OSProcess accessor primCanConnectToXDisplay: ':0.0' "
	"OSProcess accessor primCanConnectToXDisplay: ':1' "

	<primitive: 'primitiveCanConnectToXDisplay' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'display handling - old plugin compatibility' stamp: 'dtl 8/3/2003 20:34'!
oldPrimGetXDisplayName

	"Deprecated - install the XDisplayControlPlugin to eliminate the need to call this method"

	"Answer a string containing the name for the X display, or nil if the display was opened
	using the $DISPLAY environment variable. This answers the name of the X display as of
	the time it was last opened, which may be different from the current setting of $DISPLAY."

	"OSProcess accessor primGetXDisplayName"

	<primitive: 'primitiveGetXDisplayName' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'display handling - old plugin compatibility' stamp: 'dtl 8/3/2003 20:35'!
oldPrimIsConnectedToXServer

	"Deprecated - install the XDisplayControlPlugin to eliminate the need to call this method"

	"Answer true if VM is currently connected to an X server."

	"OSProcess accessor primIsConnectedToXServer inspect"
	"| x |
	OSProcess accessor primKillDisplay.
	x _ OSProcess accessor primIsConnectedToXServer.
	OSProcess accessor primOpenXDisplay.
	x inspect"

	<primitive: 'primitiveIsConnectedToXServer' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'display handling - old plugin compatibility' stamp: 'dtl 8/3/2003 20:35'!
oldPrimKillDisplay

	"Deprecated - install the XDisplayControlPlugin to eliminate the need to call this method"

	"Call an internal function which will disconnect the X display session."

	"OSProcess thisOSProcess processAccessor primKillDisplay"

	<primitive: 'primitiveKillDisplay' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'display handling - old plugin compatibility' stamp: 'dtl 8/3/2003 20:35'!
oldPrimOpenXDisplay

	"Deprecated - install the XDisplayControlPlugin to eliminate the need to call this method"

	"Call an internal function which will open the X display session."

	"OSProcess thisOSProcess processAccessor primKillDisplay.
	(Delay forSeconds: 5) wait.
	OSProcess thisOSProcess processAccessor primOpenXDisplay"

	<primitive: 'primitiveOpenXDisplay' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'display handling - old plugin compatibility' stamp: 'dtl 8/3/2003 20:35'!
oldPrimSetXDisplayName: aStringOrNil

	"Deprecated - install the XDisplayControlPlugin to eliminate the need to call this method"

	"Set the name for the X display for use in the next call to primitiveOpenXDisplay.
	aStringOrNil must be either a String (such as 'myhost:0') or nil, indicating that
	the current value of $DISPLAY should be used."

	"OSProcess accessor primSetXDisplayName: ':0.0' "
	"OSProcess accessor primSetXDisplayName: nil "

	<primitive: 'primitiveSetXDisplayName' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'display handling' stamp: 'dtl 8/3/2003 18:26'!
primCanConnectToXDisplay: xDisplayName
	"Open and close a connection to displayName. It the connection was successfully
	opened, answer true; otherwise false. This is intended to check for the ability
	to open an X display prior to actually making the attempt."

	"OSProcess accessor primCanConnectToXDisplay: ':0.0' "
	"OSProcess accessor primCanConnectToXDisplay: ':1' "

	<primitive: 'primitiveCanConnectToDisplay' module: 'XDisplayControlPlugin'>
	^ self oldPrimCanConnectToXDisplay: xDisplayName

! !

!UnixOSProcessAccessor methodsFor: 'display handling' stamp: 'dtl 8/6/2003 06:24'!
primDisconnectDisplay
	"Disconnect the X display session. The actual Squeak window on the X server is not
	effected, but this instance of Squeak will not have any further interaction with it."

	"OSProcess thisOSProcess processAccessor primDisconnectDisplay"

	<primitive: 'primitiveDisconnectDisplay' module: 'XDisplayControlPlugin'>
	^ self oldPrimKillDisplay

! !

!UnixOSProcessAccessor methodsFor: 'display handling' stamp: 'dtl 8/24/2003 10:13'!
primFlushXDisplay
	"Call an internal function to synchronize output to the X display."

	"OSProcess thisOSProcess processAccessor primFlushXDisplay"

	<primitive: 'primitiveFlushDisplay' module: 'XDisplayControlPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'display handling' stamp: 'dtl 8/3/2003 18:27'!
primGetXDisplayName
	"Answer a string containing the name for the X display, or nil if the display was opened
	using the $DISPLAY environment variable. This answers the name of the X display as of
	the time it was last opened, which may be different from the current setting of $DISPLAY."

	"OSProcess accessor primGetXDisplayName"

	<primitive: 'primitiveGetDisplayName' module: 'XDisplayControlPlugin'>
	^ self oldPrimGetXDisplayName

! !

!UnixOSProcessAccessor methodsFor: 'display handling' stamp: 'dtl 8/3/2003 18:29'!
primIsConnectedToXServer
	"Answer true if VM is currently connected to an X server."

	"OSProcess accessor primIsConnectedToXServer inspect"
	"| x |
	OSProcess accessor primKillDisplay.
	x _ OSProcess accessor primIsConnectedToXServer.
	OSProcess accessor primOpenXDisplay.
	x inspect"

	<primitive: 'primitiveIsConnectedToDisplay' module: 'XDisplayControlPlugin'>
	^ self oldPrimIsConnectedToXServer

! !

!UnixOSProcessAccessor methodsFor: 'display handling' stamp: 'dtl 8/6/2003 06:22'!
primKillDisplay
	"Disconnect the X display session and destroy the Squeak window on the X display."

	"OSProcess thisOSProcess processAccessor primKillDisplay"

	<primitive: 'primitiveKillDisplay' module: 'XDisplayControlPlugin'>
	^ self oldPrimKillDisplay

! !

!UnixOSProcessAccessor methodsFor: 'display handling' stamp: 'dtl 8/3/2003 18:31'!
primOpenXDisplay
	"Call an internal function which will open the X display session."

	"OSProcess thisOSProcess processAccessor primKillDisplay.
	(Delay forSeconds: 5) wait.
	OSProcess thisOSProcess processAccessor primOpenXDisplay"

	<primitive: 'primitiveOpenDisplay' module: 'XDisplayControlPlugin'>
	^ self oldPrimOpenXDisplay

! !

!UnixOSProcessAccessor methodsFor: 'display handling' stamp: 'dtl 8/3/2003 18:32'!
primSetXDisplayName: aStringOrNil
	"Set the name for the X display for use in the next call to primitiveOpenXDisplay.
	aStringOrNil must be either a String (such as 'myhost:0') or nil, indicating that
	the current value of $DISPLAY should be used."

	"OSProcess accessor primSetXDisplayName: ':0.0' "
	"OSProcess accessor primSetXDisplayName: nil "

	<primitive: 'primitiveSetDisplayName' module: 'XDisplayControlPlugin'>
	^ self oldPrimSetXDisplayName: aStringOrNil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 2/24/2001 14:28'!
primCanReceiveSignals: anIntegerPid
	"Answer true if an external OS process can receive signals. In most
	cases, if the process identified by anIntegerPid cannot receive signals, it is
	because the process does not exist and anIntegerPid is a stale reference (possibly
	left over from a previous Squeak session). Answer nil if the primitive does
	not exist (possibly because the VM is using an older version of the plugin)."

	<primitive: 'primitiveCanReceiveSignals' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:24'!
primSendSigabrtTo: anIntegerPid
	"Send SIGABRT (abort) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigabrtTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:24'!
primSendSigalrmTo: anIntegerPid
	"Send SIGALRM (alarm) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigalrmTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:24'!
primSendSigchldTo: anIntegerPid
	"Send SIGCHLD (child status has changed, usually death of child) to the OS process
	identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigchldTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:25'!
primSendSigcontTo: anIntegerPid
	"Send SIGCONT (continue) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigcontTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:25'!
primSendSighupTo: anIntegerPid
	"Send SIGHUP (hangup) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSighupTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:25'!
primSendSigintTo: anIntegerPid
	"Send SIGINT (interrupt) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigintTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:25'!
primSendSigkillTo: anIntegerPid
	"Send SIGKILL (kill, unblockable) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigkillTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:25'!
primSendSigpipeTo: anIntegerPid
	"Send SIGPIPE (broken pipe) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigpipeTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:25'!
primSendSigquitTo: anIntegerPid
	"Send SIGQUIT (quit) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigquitTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:25'!
primSendSigstopTo: anIntegerPid
	"Send SIGSTOP (stop, unblockable) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigstopTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:26'!
primSendSigtermTo: anIntegerPid
	"Send SIGTERM (termination) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigtermTo' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:26'!
primSendSigusr1To: anIntegerPid
	"Send SIGUSR1 (User-defined signal 1) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigusr1To' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'OS signal sending' stamp: 'dtl 7/22/2000 17:26'!
primSendSigusr2To: anIntegerPid
	"Send SIGUSR2 (User-defined signal 2) to the OS process identified by anIntegerPid.
	Answer 0 on success, -1 on failure, and nil if the pluggable primitive is not present."

	<primitive: 'primitiveSendSigusr2To' module: 'UnixOSProcessPlugin'>
	^ nil

! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 8/3/2002 20:29'!
primForwardSignal: signalNumber toSemaphore: semaphoreIndex
	"Set a signal handler in the VM which will signal a Smalltalk semaphore at
	semaphoreIndex whenever an external signal sigNum is received. Answer the
	prior value of the signal handler. If semaphoreIndex is zero, the handler is
	unregistered, and the VM returns to its default behavior for handling that
	signal.

	The Smalltalk semaphore is expected to be kept at the same index location
	indefinitely during the lifetime of a Squeak session. If that is not the case, the
	handler must be unregistered prior to unregistering the Smalltalk semaphore."

	<primitive: 'primitiveForwardSignalToSemaphore' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 7/19/2010 23:38'!
primPrintAllStacksOnSignal: signalNumber
	"Set a signal handler in the VM which will print all call stacks on the console
	output whenever an external signal sigNum is received. Answer the
	prior value of the signal handler."

	"OSProcess accessor primPrintAllStacksOnSignal: OSProcess accessor primSigUsr1Number"

	"OSProcess accessor primForwardSignal: OSProcess accessor primSigUsr1Number toSemaphore: nil"

	<primitive: 'primitivePrintAllStacksOnSignal' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 8/4/2002 01:13'!
primSemaIndexFor: sigNum
	"Answer the registration index of the semaphore currently associated with the
	signal handler for sigNum."

	<primitive: 'primitiveSemaIndexFor' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 8/3/2002 20:36'!
primSigChldNumber
	"Integer value corresponding to SIGCHLD"

	<primitive: 'primitiveSigChldNumber' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 12/6/2002 18:04'!
primSigHupNumber
	"Integer value corresponding to SIGHUP"

	<primitive: 'primitiveSigHupNumber' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 8/4/2002 00:19'!
primSigIntNumber
	"Integer value corresponding to SIGINT"

	"OSProcess accessor primSigIntNumber"

	<primitive: 'primitiveSigIntNumber' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 12/6/2002 18:05'!
primSigKillNumber
	"Integer value corresponding to SIGKILL"

	"OSProcess accessor primSigKillNumber"

	<primitive: 'primitiveSigKillNumber' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 8/3/2002 20:35'!
primSigPipeNumber
	"Integer value corresponding to SIGPIPE"

	<primitive: 'primitiveSigPipeNumber' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 12/6/2002 18:06'!
primSigQuitNumber
	"Integer value corresponding to SIGQUIT"

	<primitive: 'primitiveSigQuitNumber' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 12/6/2002 18:06'!
primSigTermNumber
	"Integer value corresponding to SIGTERM"

	<primitive: 'primitiveSigTermNumber' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 11/4/2005 06:41'!
primSigUsr1Number
	"Integer value corresponding to SIGUSR1"

	<primitive: 'primitiveSigUsr1Number' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'private - signal forwarding' stamp: 'dtl 11/4/2005 06:42'!
primSigUsr2Number
	"Integer value corresponding to SIGUSR2"

	<primitive: 'primitiveSigUsr2Number' module: 'UnixOSProcessPlugin'>
	^ nil
! !

!UnixOSProcessAccessor methodsFor: 'file lock registry' stamp: 'dtl 3/5/2005 13:10'!
register: fileRegionLock
	"If an object equal to fileRegionLock exists in the registry, answer it. Otherwise, add
	fileRegionLock to the registry and answer fileRegionLock. Caching is enabled when
	EmulateWin32FileLocking is true."

	^ self emulateWin32FileLocking
		ifTrue: [super register: fileRegionLock]
		ifFalse: [fileRegionLock]
! !

!UnixOSProcessAccessor methodsFor: 'file lock registry' stamp: 'dtl 3/5/2005 13:10'!
unregister: fileRegionLock
	"If an object equal to fileRegionLock exists in the registry, remove it and
	answer the object. Otherwise answer nil. Caching is enabled when
	EmulateWin32FileLocking is true."

	^ self emulateWin32FileLocking
		ifTrue: [super unregister: fileRegionLock]
		ifFalse: [nil]
! !

!UnixOSProcessAccessor methodsFor: 'accessing' stamp: 'dtl 1/25/2004 21:31'!
sigChldSemaphore
	"Answer the value of sigChldSemaphore"

	^ sigChldSemaphore ifNil: [sigChldSemaphore _ self forwardSigChld].
! !

!UnixOSProcessAccessor methodsFor: 'accessing' stamp: 'dtl 1/25/2004 21:29'!
sigChldSemaphore: anObject
	"Set the value of sigChldSemaphore"

	sigChldSemaphore _ anObject! !

!UnixOSProcessAccessor class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:49'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for the
	Squeak VM running on the current platform."

	^ OSProcess isUnix
! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'dtl 2/26/2002 16:11'!
activeHandles
	"Answer an Array of handles for all children that are believed to be running."

	^ OSProcess thisOSProcess activeHandles
! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'dtl 9/3/2010 13:13'!
grimReaperProcess
	"This is a process which waits for the death of a child OSProcess, and 
	informs any dependents of the change."

	grimReaper ifNil: [grimReaper _ [
				[self sigChldSemaphoreSet wait.
				self changed: #childProcessStatus] repeat] newProcess.
				grimReaper resume.
				"name selected to look reasonable in the process browser"
				grimReaper name: ((ReadStream on: grimReaper hash asString) next: 5)
						, ': the child OSProcess watcher'].
	^ grimReaper! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'dtl 9/10/2002 09:19'!
primCanAccessChildProcess: handleObject
	"Answer true if the OS process represented by a HANDLE can be accessed by this OS process."

	<primitive: 'primitiveCanAccessChildProcess' module: 'Win32OSProcessPlugin'>

	^ false! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'dtl 2/25/2002 08:23'!
primGetExitStatusForHandle: handleObject
	"Answer the exit status for the process represented by a HANDLE. Fail if
	the process is still active, or if the GetExitCodeProcess call fails."

	"| procInfo |
	procInfo _ OSProcess accessor primCommand: 'SOL'.
	(Delay forSeconds: 5) wait.
	OSProcess accessor primGetExitStatusForHandle: procInfo first"

	<primitive: 'primitiveGetExitStatusForHandle' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'dtl 2/26/2002 16:14'!
primSetWaitForAnyProcessExit: arrayOfProcessHandleObjects thenSignalSemaphoreWithIndex: index
	"Set up a thread to wait for a process HANDLE to exit, then signal the Semaphore
	at index. This provides asychronous notification of an external process exit."

	<primitive: 'primitiveSetWaitForAnyProcessExitThenSignalSemaphoreWithIndex' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'dtl 2/27/2002 11:34'!
primTerminateThread: aThreadHandle
	"Kill the thread. No cleanup is performed, so use with caution for a thread which
	is (for example) manipulating a mutex. Answer true for success, else false."

	<primitive: 'primitiveTerminateThread' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'ThierryGoubier 7/27/2017 22:39'!
restartChildWatcherProcess
	grimReaper notNil
		ifTrue: [ grimReaper terminate.
			grimReaper _ nil ].
	self grimReaperProcess! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'dtl 1/13/2007 09:48'!
restartChildWatcherThread: arrayOfProcessHandleObjects
	"Start a new child watcher thread. If a thread is alread active, terminate
	it before starting a new one."

	self childWatcherThread ifNotNil: [childWatcherThread terminate].
	self childWatcherThread: (self setWaitForAnyProcessExit: arrayOfProcessHandleObjects).
	^ childWatcherThread
! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'dtl 2/26/2002 19:08'!
setWaitForAnyProcessExit: arrayOfProcessHandleObjects
	"Set up a thread to wait for a process HANDLE to exit, then signal the Semaphore
	at index. This provides asychronous notification of an external process exit. The
	caller should close the thread handle when it is no longer needed."

	"OSProcess command: 'SOL'.
	OSProcess accessor setWaitForAnyProcessExit: OSProcess thisOSProcess activeHandles."

	| threadInfo |
	arrayOfProcessHandleObjects isEmpty ifTrue: [^ nil].
	threadInfo _ self
		primSetWaitForAnyProcessExit: arrayOfProcessHandleObjects
		thenSignalSemaphoreWithIndex: self semaIndex.
	^ WindowsThread
		threadID: threadInfo last
		handle: threadInfo first
		running: true

! !

!WindowsOSProcessAccessor methodsFor: 'child process management' stamp: 'dtl 2/25/2002 21:12'!
sigChldSemaphoreSet

	sigChldSemaphore ifNil:
		[sigChldSemaphore _ Semaphore new.
		semaIndex _ Smalltalk registerExternalObject: sigChldSemaphore].
	^ sigChldSemaphore! !

!WindowsOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 9/10/2002 09:24'!
canAccessChildProcess: anExternalProcess
	"Is the child process still there? Maybe not if we have restarted the image
	and anExternalProcess refers to a process which died while we were not
	watching."

	| handle |
	^ (handle _ anExternalProcess handle) notNil
		ifTrue: [self primCanAccessChildProcess: handle]
		ifFalse: [false]
! !

!WindowsOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 2/22/2002 22:17'!
canAccessSystem
	"Answer true if it is possible to access the external process, else false. Failure
	to access the external process is probably due to lack of a UnixOSProcessPlugin
	module."

	^ self primGetPid notNil
! !

!WindowsOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 9/26/2005 07:45'!
isExecutable: aPathName
	"Answer true if file at aPathName has execute permission for this process."

	"FIXME: Default to true for Windows"

	^ true

! !

!WindowsOSProcessAccessor methodsFor: 'testing' stamp: 'dtl 10/10/2012 08:05'!
primOneShot
	"Answer true the first time this is called in a Squeak session, and false thereafter."

	"OSProcess accessor primOneShot"

	<primitive: 'primitiveOneShot' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'accessing' stamp: 'dtl 2/27/2002 11:56'!
childWatcherThread
	"A thread which signals my sigChldSemaphore when any child process exits."

	^ childWatcherThread! !

!WindowsOSProcessAccessor methodsFor: 'accessing' stamp: 'dtl 2/27/2002 11:44'!
childWatcherThread: aThreadObject
	"A thread which signals my sigChldSemaphore when any child process exits."

	childWatcherThread _ aThreadObject
! !

!WindowsOSProcessAccessor methodsFor: 'accessing' stamp: 'dtl 2/25/2002 21:17'!
semaIndex
	"Index of the registered Semaphore"

	semaIndex ifNil: [self initialize].
	^ semaIndex! !

!WindowsOSProcessAccessor methodsFor: 'initialize - release' stamp: 'dtl 2/26/2002 08:43'!
finalize
	"Clean up grimReaper and associated semaphore."

	grimReaper ifNotNil:
			[grimReaper terminate.
			grimReaper _ nil].
	sigChldSemaphore ifNotNil:
			[Smalltalk unregisterExternalObject: sigChldSemaphore.
			sigChldSemaphore _ nil].
	semaIndex _ nil
! !

!WindowsOSProcessAccessor methodsFor: 'initialize - release' stamp: 'dtl 3/2/2002 08:33'!
initialize
	"Create and register a semaphore to be used for signaling external process exits"

	super initialize.
	self sigChldSemaphoreSet.
	self grimReaperProcess
! !

!WindowsOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 2/27/2002 13:22'!
getMainThread
	"Answer the main thread of this OS process. The handle for this thread is a
	pseudo-handle, and cannot be used to terminate the thread."

	"OSProcess accessor getMainThread"

	^ WindowsThread threadID: self primGetPid handle: self primGetPidHandle running: true
! !

!WindowsOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 9/6/2002 14:58'!
primGetCurrentWorkingDirectory
	"Call getcwd() to get the current working directory."

	"OSProcess accessor primGetCurrentWorkingDirectory"

	<primitive: 'primitiveGetCurrentWorkingDirectory' module: 'Win32OSProcessPlugin'>
	^ nil
! !

!WindowsOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 1/20/2019 13:39'!
primGetCurrentWorkingDirectoryAsBytes
	"Call getcwd() to get the current working directory."

	"OSProcess accessor primGetCurrentWorkingDirectoryAsBytes"

	<primitive: 'primitiveGetCurrentWorkingDirectoryAsBytes' module: 'Win32OSProcessPlugin'>
	self primitiveFailed
! !

!WindowsOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 2/22/2002 18:23'!
primGetEnvironmentStrings
	"Answer the environment block in the form of an Array of Strings. The
	caller is expected to parse the strings into a dictionary of keys and values."

	<primitive: 'primitiveGetEnvironmentStrings' module: 'Win32OSProcessPlugin'>
	^ nil
! !

!WindowsOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 1/20/2019 13:49'!
primGetEnvironmentStringsAsBytes
	"Answer the environment block in the form of an Array of Strings. The
	caller is expected to parse the strings into a dictionary of keys and values."

	<primitive: 'primitiveGetEnvironmentStringsAsBytes' module: 'Win32OSProcessPlugin'>
	self primitiveFailed
! !

!WindowsOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 2/27/2002 13:20'!
primGetMainThreadHandle
	"Answer a pseudo-handle for my main thread."

	"OSProcess accessor primGetMainThreadHandle"

	<primitive: 'primitiveGetMainThreadHandle' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 3/4/2006 17:57'!
primGetMainThreadID
	"Answer the ID of my main thread."

	"OSProcess accessor primGetMainThreadID"

	<primitive: 'primitiveGetMainThreadID' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 2/22/2002 16:06'!
primGetPid
	"Answer the OS process ID for the OS process in which I am currently executing."

	<primitive: 'primitiveGetPid' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'external process access' stamp: 'dtl 2/27/2002 13:21'!
primGetPidHandle
	"Answer the pseudo-handle for my OS process"

	"OSProcess accessor primGetPidHandle"

	<primitive: 'primitiveGetPidHandle' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 19:41'!
getStdErr
	"Answer an IO handle (representing a SQFile data structure in interp.c) for the standard
	error for the OS process in which I am currently executing, or nil if the IO handle
	cannot be obtained."

	"OSProcess accessor getStdErr"

	| error |
	error _ self primGetStdErrorForSession: self sessionIdentifier.
	^ (error notNil and: [error last])
		ifTrue: [error first]
		ifFalse: [nil]
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 23:51'!
getStdErrHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard error for the
	OS process in which I am currently executing."

	^ self getStdErr
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 19:45'!
getStdIn
	"Answer an IO handle (representing a SQFile data structure in interp.c) for the standard
	input for the OS process in which I am currently executing, or nil if the IO handle
	cannot be obtained."

	"OSProcess accessor getStdIn"

	| input |
	input _ self primGetStdInputForSession: self sessionIdentifier.
	^ (input notNil and: [input last])
		ifTrue: [input first]
		ifFalse: [nil]
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 23:51'!
getStdInHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard input for the
	OS process in which I am currently executing."

	^ self getStdIn
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 19:45'!
getStdOut
	"Answer an IO handle (representing a SQFile data structure in interp.c) for the standard
	output for the OS process in which I am currently executing, or nil if the IO handle
	cannot be obtained."

	"OSProcess accessor getStdOut"

	| output |
	output _ self primGetStdOutputForSession: self sessionIdentifier.
	^ (output notNil and: [output last])
		ifTrue: [output first]
		ifFalse: [nil]
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 23:51'!
getStdOutHandle
	"Answer the handle (a SQFile data structure in interp.c) for the standard output for the
	OS process in which I am currently executing."

	^ self getStdOut
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 23:41'!
openStdErr
	"Answer an IO handle (representing a SQFile data structure in interp.c) for the standard
	error for the OS process in which I am currently executing. Open a console if necessary
	to make standard error available."

	"OSProcess accessor openStdErr"

	| error |
	error _ self primGetStdErrorForSession: self sessionIdentifier.
	error ifNil: [^ nil].
	error last
		ifFalse:
			[self primAllocConsole.
			error _ self primGetStdOutputForSession: self sessionIdentifier].
	^ error first
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 23:45'!
openStdIn
	"Answer an IO handle (representing a SQFile data structure in interp.c) for the standard
	input for the OS process in which I am currently executing. Open a console if necessary
	to make standard input available."

	"OSProcess accessor openStdIn"

	| input |
	input _ self primGetStdInputForSession: self sessionIdentifier.
	input ifNil: [^ nil].
	input last
		ifFalse:
			[self primAllocConsole.
			input _ self primGetStdOutputForSession: self sessionIdentifier].
	^ input first
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/19/2022 19:46'!
openStdOut
	"Answer an IO handle (representing a SQFile data structure in interp.c) for the standard
	output for the OS process in which I am currently executing. Open a console if necessary
	to make standard input available."

	"OSProcess accessor openStdOut"

	| output |
	output _ self primGetStdOutputForSession: self sessionIdentifier.
	output ifNil: [^ nil].
	output last
		ifFalse:
			[self primAllocConsole.
			output _ self primGetStdOutputForSession: self sessionIdentifier].
	^ output first
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/20/2022 00:50'!
setStdErr: aFileStream
	"Set the standard error handle for this OSProcess to be that of aFileStream"

	"| fs |
	fs _ FileStream fileNamed: 'stdError.tmp'.
	OSProcess accessor setStdErr: fs"

	aFileStream ifNil: [^ false].
	^ self primSetStdErr: aFileStream fileID
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/20/2022 00:51'!
setStdIn: aFileStream
	"Set the standard input handle for this OSProcess to be that of aFileStream"

	"| fs |
	fs _ FileStream fileNamed: 'stdInput.tmp'.
	fs nextPutAll: 'this is a line of text'; cr.
	fs position: 0.
	OSProcess accessor setStdIn: fs"

	aFileStream ifNil: [^ false].
	^ self primSetStdIn: aFileStream fileID
! !

!WindowsOSProcessAccessor methodsFor: 'standard IO handles' stamp: 'dtl 8/20/2022 00:51'!
setStdOut: aFileStream
	"Set the standard output handle for this OSProcess to be that of aFileStream"

	"| fs |
	fs _ FileStream fileNamed: 'stdOutput.tmp'.
	OSProcess accessor setStdOut: fs"

	aFileStream ifNil: [^ false].
	^ self primSetStdOut: aFileStream fileID
! !

!WindowsOSProcessAccessor methodsFor: 'nonblocking read' stamp: 'dtl 10/1/2005 09:26'!
lastReadFor: aSemaphoreIndex
	"A character has been read into an external buffer corresponding to aSemaphoreIndex,
	and is now available. Answer integer value of the character, or nil if no character
	was read, or -1 if an error occurred on the read."

	| c readResult |
	readResult _ Array new: 3.
	c _ self primLastReadFor: aSemaphoreIndex storeIn: readResult.
	(c < 1) ifTrue: [self error: 'primLastReadFor: error, returned negative value'].
	^ c
! !

!WindowsOSProcessAccessor methodsFor: 'nonblocking read' stamp: 'dtl 10/1/2005 09:26'!
lastReadFor: aSemaphoreIndex storeIn: aThreeElementArray
	"A character has been read into an external buffer corresponding to aSemaphoreIndex,
	and is now available. Answer integer value of the character, or nil if no character
	was read, or -1 if an error occurred on the read. The results of the read call are
	stored in aThreeElementArray as a side effect."

	^ self primLastReadFor: aSemaphoreIndex storeIn: aThreeElementArray
! !

!WindowsOSProcessAccessor methodsFor: 'nonblocking read' stamp: 'dtl 8/20/2022 00:50'!
nextFrom: aFileStream signaling: aSemaphoreIndex
	"Read the next character from aFileStream into a buffer in the VM. When the read completes,
	signal the specified Semaphore to notify that the character is available."

	^ self primNextFrom: aFileStream fileID signaling: aSemaphoreIndex
! !

!WindowsOSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 11:46'!
osppModuleName
	"Answer a string containing the module name string for the OSPP plugin."

	"OSProcess accessor osppModuleName"

	^ self primOSProcessPluginModuleName
! !

!WindowsOSProcessAccessor methodsFor: 'plugin identification' stamp: 'dtl 10/1/2005 11:46'!
osppModuleVersionString
	"Answer a string containing the version string for the OSPP plugin."

	"OSProcess accessor osppModuleVersionString"

	^ self primOSProcessPluginModuleVersionString
! !

!WindowsOSProcessAccessor methodsFor: 'console' stamp: 'dtl 3/25/2002 06:28'!
primAllocConsole
	"Allocate a console if not already allocated."

	"OSProcess accessor primAllocConsole"

	<primitive: 'primitiveAllocConsole' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'console' stamp: 'dtl 3/25/2002 06:27'!
primFreeConsole
	"Deallocate the console if allocated."

	"OSProcess accessor primFreeConsole"

	<primitive: 'primitiveFreeConsole' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 8/17/2002 12:46'!
primBufferValuesAt: aSemaphoreIndex
	"For debugging only. Answer the current values of readCharBufferArray,
	readCharCountArray, and readCharStatusArray at index, an integer corresponding
	to a semaphore for one read handler thread. Answer an Array with the buffered
	character, the character count, and the status value."

	"OSProcess accessor primBufferValuesAt: 1"

	<primitive: 'primitiveBufferValuesAt' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 9/7/2002 14:55'!
primGetStdErrorForSession: sessionIdentifierByteArray
	"Answer a two element array containing the sqFile data structure representing
	standard error stream for my OS process, and a flag (true or false) to indicate
	whether the sqFile data structure contains a valid HANDLE. If no standard error
	stream is available for this OS process, the sqFile data structure will contain an
	invalid HANDLE value, which will result in failures on subsequent accesses."

	"OSProcess accessor primGetStdError"

	<primitive: 'primitiveGetStdError' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 9/7/2002 14:55'!
primGetStdInputForSession: sessionIdentifierByteArray
	"Answer a two element array containing the sqFile data structure representing
	standard input stream for my OS process, and a flag (true or false) to indicate
	whether the sqFile data structure contains a valid HANDLE. If no standard input
	stream is available for this OS process, the sqFile data structure will contain an
	invalid HANDLE value, which will result in failures on subsequent accesses."

	"OSProcess accessor primGetStdInput"

	<primitive: 'primitiveGetStdInput' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 9/7/2002 14:55'!
primGetStdOutputForSession: sessionIdentifierByteArray
	"Answer a two element array containing the sqFile data structure representing
	standard output stream for my OS process, and a flag (true or false) to indicate
	whether the sqFile data structure contains a valid HANDLE. If no standard output
	stream is available for this OS process, the sqFile data structure will contain an
	invalid HANDLE value, which will result in failures on subsequent accesses."

	"OSProcess accessor primGetStdOutput"

	<primitive: 'primitiveGetStdOutput' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/1/2005 09:26'!
primLastReadFor: aSemaphoreIndex
	"A character has been read into an external buffer corresponding to aSemaphoreIndex,
	and is now available. Answer integer value of the character, or nil if no character
	was read, or -1 if an error occurred on the read."

	<primitive: 'primitiveLastReadFor' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/1/2005 09:27'!
primLastReadFor: aSemaphoreIndex storeIn: aThreeElementArray
	"A character has been read into an external buffer corresponding to aSemaphoreIndex,
	and is now available. Answer integer value of the character, or nil if no character
	was read, or -1 if an error occurred on the read. Contents of the aThreeElementArray
	will be status of the read call, character read, and character count (which should
	always be 1)."

	<primitive: 'primitiveLastReadForStoreIn' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 4/1/2002 10:58'!
primNextFrom: anIOHandle signaling: aSemaphoreIndex
	"Read the next character from anIOHandle (a SQFile struct) into a buffer in the VM. When
	the read completes, signal the specified Semaphore to notify that the character is available."

	<primitive: 'primitiveNextFromSignaling' module: 'Win32OSProcessPlugin'>

	^ nil

! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/1/2005 11:50'!
primOSProcessPluginModuleName
	"Answer a string containing the module name string for the OSPP plugin."

	"OSProcess accessor primOSProcessPluginModuleName"

	<primitive: 'primitiveModuleName' module: 'Win32OSProcessPlugin'>
	^ nil
! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 10/1/2005 11:51'!
primOSProcessPluginModuleVersionString
	"Answer a string containing the version string for the OSPP plugin."

	"OSProcess accessor primOSProcessPluginModuleVersionString"

	<primitive: 'primitiveVersionString' module: 'Win32OSProcessPlugin'>
	^ nil
! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 1/31/2020 00:15'!
primSpaceFor: byteSize InDirectoryPath: dirPath

	<primitive: 'primitiveSpaceForByteSizeInDirectoryPath' module: 'Win32OSProcessPlugin'>
	self flag: 'FIXME'. "not yet implemented in OSPP for Windows"
	^ self primitiveFailed! !

!WindowsOSProcessAccessor methodsFor: 'private - primitive access' stamp: 'dtl 4/8/2007 10:54'!
primTestEndOfFileFlag: aSqFileStruct
	"Answer whether the file represented by aSqFileStruct is at end of file, as determined
	by a call to feof(). This is different from StandardFileStream>>primAtEnd: which answers
	true if the file pointer is at the end of the file, but which does not call feof() to
	determine that an end of file condition has occurred. The difference is significant
	if aSqFileStruct represents a pipe or a device file, which may not be positionable
	in the sense of a conventional disk file."

	<primitive: 'primitiveTestEndOfFileFlag' module: 'Win32OSProcessPlugin'>
	self flag: 'FIXME'. "not yet implemented in OSPP for Windows"
	^ self primitiveFailed
! !

!WindowsOSProcessAccessor methodsFor: 'handles' stamp: 'dtl 2/25/2002 07:37'!
primCloseHandle: handleObject
	"Close the specified handle, which may refer to a process, a thread, or
	some other Win32 object."

	"| procInfo |
	procInfo _ OSProcess accessor primCommand: 'SOL'.
	OSProcess accessor primCloseHandle: procInfo first"

	<primitive: 'primitiveCloseHandle' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'handles' stamp: 'dtl 3/29/2002 17:47'!
primSetStdErr: anIOHandle
	"Set the standard error handle to that of anIOHandle, where anIOHandle is a
	ByteArray representation of a SQFile structure."

	<primitive: 'primitiveSetStdErr' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'handles' stamp: 'dtl 3/29/2002 17:48'!
primSetStdIn: anIOHandle
	"Set the standard input handle to that of anIOHandle, where anIOHandle is a
	ByteArray representation of a SQFile structure."

	<primitive: 'primitiveSetStdIn' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'handles' stamp: 'dtl 3/29/2002 17:48'!
primSetStdOut: anIOHandle
	"Set the standard output handle to that of anIOHandle, where anIOHandle is a
	ByteArray representation of a SQFile structure."

	<primitive: 'primitiveSetStdOut' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'deprecated' stamp: 'dtl 9/9/2002 09:24'!
primCommand: commandString
	"Run a command in a new external process. Answer a result array with
	hProcess, hThread, dwProcessId, dwThreadId.

	This primitive has been replaced by #primCommand:stdIn:stdOut:stdErr:, and
	will be removed in future versions of OSProcess."

	"OSProcess accessor primCommand: 'C:\WINDOWS\SOL'"
	"OSProcess accessor primCommand: 'SOL'"
	"OSProcess accessor primCommand: 'NoSuchProgram'"

	<primitive: 'primitiveCommand' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'process creation' stamp: 'dtl 9/9/2002 09:19'!
primCommand: commandString stdIn: inputFileIoHandle stdOut: outputFileIoHandle stdErr: errorFileIoHandle

	"Run a command in a new external process. The standard input, output and error
	stream handles are sqFile byte arrays (not Win32 HANDLE arrays), and may be nil.
	Answer a result array with hProcess, hThread, dwProcessId, dwThreadId."

	"OSProcess accessor primCommand: 'C:\WINDOWS\SOL' stdIn: nil stdOut: nil stdErr: nil"
	"OSProcess accessor primCommand: 'SOL' stdIn: nil stdOut: nil stdErr: nil"
	"OSProcess accessor primCommand: 'NoSuchProgram' stdIn: nil stdOut: nil stdErr: nil"
	"OSProcess accessor primCommand: 'SOL' stdIn: (FileStream fileNamed: 'output.tmp') fileID stdOut: nil stdErr: nil"

	<primitive: 'primitiveCommandWithInputOutputError' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'pipe open' stamp: 'dtl 11/28/2010 12:51'!
primCreatePipe
	"Create a pipe, and answer an array of two file handles (SQFile data structures in interp.c)
	for the pipe reader and writer."

	<primitive: 'primitiveCreatePipe' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'pipe open' stamp: 'dtl 11/28/2010 12:52'!
primCreatePipeWithSessionIdentifier: aByteArray
	"Create a pipe, and answer an array of two file handles (SQFile data structures in interp.c)
	for the pipe reader and writer."

	<primitive: 'primitiveCreatePipeWithSessionIdentifier' module: 'Win32OSProcessPlugin'>
	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'session identification' stamp: 'dtl 3/2/2002 08:15'!
primGetSession
	"Answer the unique identifier for this session of Smalltalk running in this OS Process."

	"OSProcess accessor primGetSession"

	<primitive: 'primitiveGetSession' module: 'Win32OSProcessPlugin'>

	^ nil! !

!WindowsOSProcessAccessor methodsFor: 'file control' stamp: 'dtl 9/26/2005 07:35'!
setBlocking: anIOHandle
	"Convert anIOHandle to an SQFile data structure and call primitive to set for blocking I/O."

	"FIXME: need to implement this for Win32"
	"self notify: 'there is no general mechanism to set blocking IO on Win32'"
! !

!WindowsOSProcessAccessor methodsFor: 'file control' stamp: 'dtl 9/26/2005 07:36'!
setNonBlocking: anIOHandle
	"Convert anIOHandle to an SQFile data structure and call primitive to set it non-blocking."

	"FIXME: need to implement this for Win32"
	"self notify: 'there is no general mechanism to set nonblocking IO on Win32'"

! !

!WindowsOSProcessAccessor class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:50'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for the
	Squeak VM running on the current platform."

	^ OSProcess isWindows
! !

!AioEventHandler methodsFor: 'aio event forwarding' stamp: 'dtl 7/4/2003 19:10'!
aioDisable: ioDescriptor
	"Definitively disable asynchronous event notification for a descriptor. The ioDescriptor
	parameter is an object representing a low level OS file or socket descriptor."

	^ self primAioDisable: ioDescriptor
! !

!AioEventHandler methodsFor: 'aio event forwarding' stamp: 'dtl 9/1/2003 16:58'!
aioEnable: ioDescriptor forSemaphore: semaphoreIndex externalObject: trueOrFalse
	"Enable asynchronous notification for a descriptor. Send this message one time
	prior to beginning event handling for ioDescriptor. The ioDescriptor parameter is an
	object representing a low level OS file or socket descriptor. The semaphoreIndex is
	the index of a Semaphore to be notified, and the third parameter is a flag indicating
	that ioDescriptor represents an external object which should not be closed on termination
	of aio handling."

	^ (self primAioEnable: ioDescriptor forSemaphore: semaphoreIndex externalObject: trueOrFalse)
			ifNil: [self notify: 'aio event forwarding not supported']
! !

!AioEventHandler methodsFor: 'aio event forwarding' stamp: 'dtl 7/4/2003 18:52'!
aioHandle: ioDescriptor exceptionEvents: exceptionFlag readEvents: readFlag writeEvents: writeFlag
	"Enable asynchronous notification for a descriptor. Send this message one time to
	enable a single event notification. Send it again after each event has been received
	and handled (in other words, the process which waits on the event semaphore is
	responsible for re-enabling the handler by calling this method each time an event
	is handled). The ioDescriptor parameter is an object representing a low level OS file or
	socket descriptor. The remaining three parameters are Boolean flags representing the
	types of events for which notification is being requested: handle exceptions, handle
	for read, and handle for write. It is common to watch for read events and exception
	events, or to watch for write events and exception events."

	^ self primAioHandle: ioDescriptor
		exceptionEvents: exceptionFlag
		readEvents: readFlag
		writeEvents: writeFlag
! !

!AioEventHandler methodsFor: 'aio event forwarding' stamp: 'dtl 7/4/2003 19:10'!
aioSuspend: ioDescriptor exceptionEvents: exceptionFlag readEvents: readFlag writeEvents: writeFlag

	"Temporarily suspend asynchronous event notification for a descriptor. The
	ioDescriptor parameter is an object representing a low level OS file or socket
	descriptor. The remaining three parameters are Boolean flags representing the
	types of events for which notification is being requested: handle exceptions,
	handle for read, and handle for write."

	^ self primAioSuspend: ioDescriptor
		exceptionEvents: exceptionFlag
		readEvents: readFlag
		writeEvents: writeFlag
! !

!AioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 8/20/2006 18:43'!
close
	"When the FileStream or Socket handled by this aio handler is closed,
	it should send #close to this handler."

	| p |
	self breakDependents.
	Smalltalk unregisterExternalObject: semaphore.
	self aioDisable: self descriptor.
	semaphore _ nil.
	semaIndex _ nil.
	p _ handlerProc.
	handlerProc _ nil.
	p ifNotNil: [p terminate] "p may be the active process, do this last"
! !

!AioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 7/5/2003 09:15'!
for: aSocketOrFileStream
	"Answer an event handler for any kind of IO stream that can be associated
	with an OS handle for aio events. Currently, subclasses of FileStream and
	Socket can have aio event handlers."

	(aSocketOrFileStream isKindOf: Socket)
		ifTrue: [^ self forSocket: aSocketOrFileStream].
	(aSocketOrFileStream isKindOf: FileStream)
		ifTrue: [^ self forFileStream: aSocketOrFileStream].
	self error: 'expected a FileStream or Socket subclass'! !

!AioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 7/5/2003 11:03'!
for: aSocketOrFileStream exceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag
	"Answer an event handler for any kind of IO stream that can be associated
	with an OS handle for aio events. Currently, subclasses of FileStream and
	Socket can have aio event handlers."

	(aSocketOrFileStream isKindOf: Socket)
		ifTrue: [^ self forSocket: aSocketOrFileStream
					exceptions: exceptionEventFlag
					readEvents: readEventFlag
					writeEvents: writeEventFlag].
	(aSocketOrFileStream isKindOf: FileStream)
		ifTrue: [^ self forFileStream: aSocketOrFileStream
					exceptions: exceptionEventFlag
					readEvents: readEventFlag
					writeEvents: writeEventFlag].
	self error: 'expected a FileStream or Socket subclass'! !

!AioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 8/19/2022 23:25'!
forFileStream: aFileStream

	self descriptor: (self descriptorForFile: aFileStream).
	self setDefaultEventMask.
! !

!AioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 8/19/2022 23:25'!
forFileStream: aFileStream exceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag

	self descriptor: (self descriptorForFile: aFileStream).
	self initializeForExceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag

! !

!AioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 8/19/2022 23:27'!
forSocket: aSocket
	"Any existing event handling for aSocket will be disabled. Note that this
	will make the socket useless for any applications that expect the prior
	event handling behavior."

	self descriptor: (self desriptorForSocket: aSocket).
	self aioDisable: self descriptor.
	self setDefaultEventMask.
! !

!AioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 8/19/2022 23:27'!
forSocket: aSocket exceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag
	"Any existing event handling for aSocket will be disabled. Note that this
	will make the socket useless for any applications that expect the prior
	event handling behavior."

	self descriptor: (self desriptorForSocket: aSocket).
	self aioDisable: self descriptor.
	self initializeForExceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag
! !

!AioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 1/23/2022 11:31'!
initializeForExceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag

	semaphore _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: semaphore.
	(AioPluginPresent = true) ifTrue: [
		([self aioEnable: self descriptor forSemaphore: self semaIndex externalObject: true]
			on: Warning
			do: [:e |
				self close. "unregister the semaphore"
				OSProcess trace: e messageText asString. nil])
			ifNotNil:
				[handlerProc _ self
					handleExceptions: exceptionEventFlag
					readEvents: readEventFlag
					writeEvents: writeEventFlag]]
! !

!AioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 7/9/2005 14:00'!
setDefaultEventMask
	"Default initialization for read events and exception events"

	^ self initializeForExceptions: true readEvents: true writeEvents: false

! !

!AioEventHandler methodsFor: 'handler process' stamp: 'dtl 7/5/2003 10:50'!
defaultHandlerProcess
	"Generate a #changed notification whenever an external aio event occurs"

	^ self handleReadAndExceptionsEvents! !

!AioEventHandler methodsFor: 'handler process' stamp: 'dtl 11/12/2011 21:25'!
handleExceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag
	"Generate a #changed notification whenever the requested type of external aio event occurs."

	| p sema handler |
	sema _ Semaphore new.
	handler _ [[self hasValidHandler] whileTrue:
		[self aioHandle: self descriptor
			exceptionEvents: exceptionEventFlag
			readEvents: readEventFlag
			writeEvents: writeEventFlag.
		sema signal. "event handler is ready"
		self semaphore wait.
		self changed]].
	p _ handler forkAt: Processor userBackgroundPriority.
	sema wait. "until process has been started and events are being handled"
	^p! !

!AioEventHandler methodsFor: 'handler process' stamp: 'dtl 7/5/2003 10:49'!
handleReadAndExceptionsEvents
	"Generate a #changed notification whenever data is available for reading or an exception
	occurs on the external IO channel."

	^ self handleExceptions: true readEvents: true writeEvents: false
! !

!AioEventHandler methodsFor: 'handler process' stamp: 'dtl 7/5/2003 10:49'!
handleReadEvents
	"Generate a #changed notification whenever data is available for reading"

	^ self handleExceptions: false readEvents: true writeEvents: false
! !

!AioEventHandler methodsFor: 'handler process' stamp: 'dtl 4/23/2012 06:45'!
hasValidHandler
	"True if the event handler is running, and if it refers to the correct external
	object semaphore. For protection following an image restart."

	^ semaIndex notNil
		and: [Smalltalk externalObjects size >= semaIndex
			and: [(Smalltalk externalObjects at: semaIndex) == semaphore]]! !

!AioEventHandler methodsFor: 'accessing' stamp: 'dtl 3/30/2003 19:05'!
descriptor

	^ descriptor! !

!AioEventHandler methodsFor: 'accessing' stamp: 'dtl 3/30/2003 19:05'!
descriptor: aLowLevelIODescriptor

	descriptor _ aLowLevelIODescriptor! !

!AioEventHandler methodsFor: 'accessing' stamp: 'dtl 3/30/2003 19:04'!
handlerProc

	^ handlerProc! !

!AioEventHandler methodsFor: 'accessing' stamp: 'dtl 3/30/2003 19:04'!
semaIndex

	^ semaIndex! !

!AioEventHandler methodsFor: 'accessing' stamp: 'dtl 3/30/2003 19:04'!
semaphore

	^ semaphore! !

!AioEventHandler methodsFor: 'finalization' stamp: 'dtl 9/4/2003 06:54'!
finalize
	"Note: An aio handler will not be garbage collected until the semaphore
	is unregistered. When the FileStream or Socket handled by this aio
	handler is closed, it should send #close to this handler."

	self close
! !

!AioEventHandler methodsFor: 'private' stamp: 'dtl 8/19/2022 23:24'!
descriptorForFile: aFileStream

	"self new descriptorForFile: SourceFiles first"

	^ self primOSFileHandle: aFileStream fileID
! !

!AioEventHandler methodsFor: 'private' stamp: 'dtl 8/19/2022 23:25'!
desriptorForSocket: aSocket

	"self new desriptorForSocket: Socket newTCP"

	^ self primOSSocketHandle: aSocket socketHandle
! !

!AioEventHandler methodsFor: 'private' stamp: 'dtl 9/4/2003 06:40'!
handleForFile: aFileStream

	"self new handleForFile: SourceFiles first"

	| ioHandle |
	ioHandle _ self useIOHandle
		ifTrue: [aFileStream ioHandle]
		ifFalse: [aFileStream fileID].
	^ self primOSFileHandle: ioHandle
! !

!AioEventHandler methodsFor: 'private' stamp: 'dtl 7/4/2003 15:16'!
handleForSocket: aSocket

	"self new handleForSocket: Socket newTCP"

	| ioHandle |
	ioHandle _ self useIOHandle
		ifTrue: [aSocket ioHandle]
		ifFalse: [aSocket socketHandle].
	^ self primOSSocketHandle: ioHandle
! !

!AioEventHandler methodsFor: 'private' stamp: 'dtl 3/30/2003 19:26'!
useIOHandle

	^ Smalltalk hasClassNamed: #IOHandle! !

!AioEventHandler methodsFor: 'primitive access' stamp: 'dtl 9/1/2003 17:53'!
primAioDisable: aDescriptor
	"Definitively disable asynchronous event notification for a descriptor. The descriptor
	parameter is an object representing a low level OS file or socket descriptor."

	<primitive: 'primitiveAioDisable' module: 'AioPlugin'>
	^ nil
! !

!AioEventHandler methodsFor: 'primitive access' stamp: 'dtl 9/1/2003 17:53'!
primAioEnable: aDescriptor forSemaphore: semaphoreIndex externalObject: trueOrFalse
	"Enable asynchronous notification for a descriptor. The descriptor parameter is an
	object representing a low level OS file or socket descriptor. The semaphoreIndex
	is the index of a Semaphore to be notified, and the third parameter is a flag indicating
	that descriptor represents an external object which should not be closed on termination
	of aio handling."

	<primitive: 'primitiveAioEnable' module: 'AioPlugin'>
	^ nil
! !

!AioEventHandler methodsFor: 'primitive access' stamp: 'dtl 9/1/2003 17:53'!
primAioHandle: aDescriptor exceptionEvents: exceptionFlag readEvents: readFlag writeEvents: writeFlag
	"Enable asynchronous notification for a descriptor. The descriptor parameter is an
	object representing a low level OS file or socket descriptor. The second parameter is
	the index of a Semaphore to be notified, the remaining three parameters are Boolean
	flags representing the types of events for which notification is being requested:
	handle exceptions, handle for read, and handle for write. It is common to watch for
	read events and exception events, or to watch for write events and exception events."

	<primitive: 'primitiveAioHandle' module: 'AioPlugin'>
	^ nil
! !

!AioEventHandler methodsFor: 'primitive access' stamp: 'dtl 9/1/2003 17:26'!
primAioModuleName
	"Module name of the installed plugin, if any."

	"self new primAioModuleName"

	<primitive: 'primitiveModuleName' module: 'AioPlugin'>
	^ nil
! !

!AioEventHandler methodsFor: 'primitive access' stamp: 'dtl 9/1/2003 17:28'!
primAioModuleVersionString
	"Module name of the installed plugin, if any."

	"self new primAioModuleVersionString"

	<primitive: 'primitiveVersionString' module: 'AioPlugin'>
	^ nil
! !

!AioEventHandler methodsFor: 'primitive access' stamp: 'dtl 9/1/2003 17:54'!
primAioSuspend: aDescriptor exceptionEvents: exceptionFlag readEvents: readFlag writeEvents: writeFlag
	"Temporarily suspend asynchronous event notification for a descriptor. The
	descriptor parameter is an object representing a low level OS file or socket
	descriptor. The remaining three parameters are Boolean flags representing the
	types of events for which notification is being requested: handle exceptions,
	handle for read, and handle for write."

	<primitive: 'primitiveAioSuspend' module: 'AioPlugin'>
	^ nil
! !

!AioEventHandler methodsFor: 'primitive access' stamp: 'dtl 9/1/2003 17:54'!
primOSFileHandle: sqFile
	"Answer the low level file descriptor for a file IO handle."

	<primitive: 'primitiveOSFileHandle' module: 'AioPlugin'>
	^ nil
! !

!AioEventHandler methodsFor: 'primitive access' stamp: 'dtl 9/1/2003 17:54'!
primOSSocketHandle: sqFile
	"Answer the low level socket descriptor for a socket IO handle."

	<primitive: 'primitiveOSSocketHandle' module: 'AioPlugin'>
	^ nil
! !

!AioEventHandler class methodsFor: 'system startup' stamp: 'dtl 11/25/2006 10:04'!
startUp: resuming
	"Clear the value of AioPluginPresent. The value will be set once when
	#aioPluginPresent is first sent, and will remain set to that value for the
	duration of this Squeak session.
	This method is called by ThisOSProcess>>startUp. AioEventHandler does
	not need to be registered in the system startup list."

	resuming ifTrue: [AioPluginPresent _ nil]
! !

!AioEventHandler class methodsFor: 'testing' stamp: 'dtl 12/18/2016 18:06'!
aioPluginPresent
	"Answer true if an AIO plugin is available. The value of AioPluginPresent
	is cleared at startup time, and is reestablished once for each Squeak session.
	See initializeAioPluginPresent to initialize after an image startup if warning
	dialog is not desired."

	"AioEventHandler aioPluginPresent"

	^ AioPluginPresent
		ifNil: [AioPluginPresent _ self basicNew primAioModuleVersionString notNil.
			AioPluginPresent ifFalse:
				[OSProcess trace: ' AioPlugin not present, AioEventHandler will use polling input'].
			^ AioPluginPresent]! !

!AioEventHandler class methodsFor: 'testing' stamp: 'dtl 10/3/2012 20:24'!
initializeAioPluginPresent
	"Initialize the AioPluginPresent flag silently without invoking a warning
	if the plugin is not present, and answer the value of the flag. Send this
	to prevent a warning dialog from being presented after image startup,
	as may be preferred if the image is to be run headless."

	"AioEventHandler initializeAioPluginPresent"

	^ AioPluginPresent
		ifNil: [AioPluginPresent _ self basicNew primAioModuleVersionString notNil]! !

!AioEventHandler class methodsFor: 'instance creation' stamp: 'dtl 7/5/2003 09:16'!
for: aSocketOrFileStream

	"self for: SourceFiles first"
	"self for: Socket new"
	"self for: OSProcess thisOSProcess stdIn"

	^ self new for: aSocketOrFileStream! !

!AioEventHandler class methodsFor: 'instance creation' stamp: 'dtl 7/5/2003 11:14'!
for: aSocketOrFileStream exceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag
	"Flag parameters are true or false, indicating types of events to be handled."

	^ self new for: aSocketOrFileStream
			exceptions: exceptionEventFlag
			readEvents: readEventFlag
			writeEvents: writeEventFlag! !

!AioEventHandler class methodsFor: 'instance creation' stamp: 'dtl 7/4/2003 18:47'!
forFileStream: aFileStream

	"self forFileStream: SourceFiles first"

	^ self new forFileStream: aFileStream

! !

!AioEventHandler class methodsFor: 'instance creation' stamp: 'dtl 7/5/2003 11:14'!
forFileStream: aFileStream exceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag
	"Flag parameters are true or false, indicating types of events to be handled."

	"self forFileStream: SourceFiles first"

	^ self new forFileStream: aFileStream
			exceptions: exceptionEventFlag
			readEvents: readEventFlag
			writeEvents: writeEventFlag
! !

!AioEventHandler class methodsFor: 'instance creation' stamp: 'dtl 7/4/2003 19:01'!
forSocket: aSocket

	"self forSocket: Socket new"

	^ self new forSocket: aSocket

! !

!AioEventHandler class methodsFor: 'instance creation' stamp: 'dtl 7/5/2003 11:14'!
forSocket: aSocket exceptions: exceptionEventFlag readEvents: readEventFlag writeEvents: writeEventFlag
	"Flag parameters are true or false, indicating types of events to be handled."

	"self forSocket: Socket new"

	^ self new forSocket: aSocket
			exceptions: exceptionEventFlag
			readEvents: readEventFlag
			writeEvents: writeEventFlag
! !

!PseudoAioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 11/25/2006 10:49'!
close
	"When the FileStream or Socket handled by this aio handler is closed,
	it should send #close to this handler."

	eventGenerator ifNotNil: [eventGenerator terminate]! !

!PseudoAioEventHandler methodsFor: 'initialize-release' stamp: 'RB 9/3/2010 11:52'!
eventGeneratorProcess
	"A process that generates periodic #changed events"

	| d p|
	d _ Delay forMilliseconds: 125.
	p _ [[self changed.
	d wait] repeat] newProcess.
	^p resume.! !

!PseudoAioEventHandler methodsFor: 'initialize-release' stamp: 'dtl 11/25/2006 13:15'!
initialize

	self eventGenerator.
	^ super initialize! !

!PseudoAioEventHandler methodsFor: 'accessing' stamp: 'dtl 11/25/2006 13:17'!
eventGenerator
	"Answer the value of eventGenerator"

	^ eventGenerator ifNil: [eventGenerator _ self eventGeneratorProcess]! !

!AttachableFileStream methodsFor: 'converting' stamp: 'dtl 8/20/2022 01:00'!
asAsyncFileReadStream
	"Answer a replacement for this object, with asynchronous event handling.
	Do not close the fileID when this object is finalized."

	self keepOpen.
	^ AsyncFileReadStream
		name: self name
		attachTo: fileID
		writable: self isReadOnly not
! !

!AttachableFileStream methodsFor: 'converting' stamp: 'dtl 9/16/2002 17:59'!
asAttachableFileStream

	^ self
! !

!AttachableFileStream methodsFor: 'converting' stamp: 'dtl 8/20/2022 01:00'!
asBufferedAsyncFileReadStream
	"Answer a replacement for this object, with asynchronous event handling
	and buffered output. Do not close the fileID when this object is finalized."

	self keepOpen.
	^ BufferedAsyncFileReadStream
		name: self name
		attachTo: fileID
		writable: self isReadOnly not
! !

!AttachableFileStream methodsFor: 'finalization' stamp: 'dtl 9/17/2002 08:08'!
autoClose
	"Private. Answer true if the file should be automatically closed when 
	this object is finalized."

	^ autoClose
		ifNil: [autoClose _ true]! !

!AttachableFileStream methodsFor: 'finalization' stamp: 'dtl 7/6/2006 22:17'!
finalize

	self autoClose
		ifTrue: [[self primCloseNoError: fileID] on: Error do: []]! !

!AttachableFileStream methodsFor: 'finalization' stamp: 'dtl 9/17/2002 08:05'!
keepOpen
	"Do not allow the file to be closed when this object is finalized."

	autoClose _ false
! !

!AttachableFileStream methodsFor: 'open/close' stamp: 'dtl 8/20/2022 00:36'!
close
	"Close this file."

	fileID ifNotNil:
		[self primCloseNoError: fileID.
			self unregister.
			fileID _ nil]
! !

!AttachableFileStream methodsFor: 'open/close' stamp: 'dtl 6/12/1999 16:00'!
ensureOpen

	self shouldNotImplement
! !

!AttachableFileStream methodsFor: 'open/close' stamp: 'dtl 6/12/1999 16:00'!
open

	self shouldNotImplement
! !

!AttachableFileStream methodsFor: 'open/close' stamp: 'dtl 6/12/1999 16:01'!
open: fileName forWrite: writeMode

	self shouldNotImplement
! !

!AttachableFileStream methodsFor: 'open/close' stamp: 'dtl 6/12/1999 16:02'!
openReadOnly

	self shouldNotImplement
! !

!AttachableFileStream methodsFor: 'open/close' stamp: 'dtl 6/12/1999 16:02'!
reopen

	self shouldNotImplement
! !

!AttachableFileStream methodsFor: 'initialize-release' stamp: 'dtl 4/14/2006 09:34'!
disableEventHandling
	"Subclasses may disable event handling"! !

!AttachableFileStream methodsFor: 'read, write, position' stamp: 'dtl 8/20/2022 00:36'!
flush
	"Flush the external OS stream (the one in the C library)."

	OSProcess accessor flushExternalStream: fileID! !

!AttachableFileStream methodsFor: 'read, write, position' stamp: 'dtl 12/9/2022 22:01:51'!
nextPutAll: aString
	^ super nextPutAll: aString asByteString! !

!AttachableFileStream methodsFor: 'read, write, position' stamp: 'dtl 5/17/2009 14:11'!
position
	"Return the receiver's current file position. If the stream is not positionable,
	as in the case of a Unix pipe stream, answer 0."

	^ [super position]
		on: Error
		do: [0]
! !

!AttachableFileStream methodsFor: 'read, write, position' stamp: 'dtl 9/2/2006 17:17'!
upToEnd
	"Answer a subcollection from the current access position through the last element
	of the receiver. This is slower than the method in StandardFileStream, but it
	works with pipes which answer false to #atEnd when no further input is
	currently available, but the pipe is not yet closed."

	| newStream buffer nextBytes |
	buffer _ buffer1 species new: 1000.
	newStream _ WriteStream on: (buffer1 species new: 100).
	[self atEnd or: [(nextBytes _ self nextInto: buffer) isEmpty]]
		whileFalse: [newStream nextPutAll: nextBytes].
	^ newStream contents
! !

!AttachableFileStream methodsFor: 'read, write, position' stamp: 'dtl 6/4/2006 16:02'!
upToEndOfFile
	"Answer a subcollection from the current access position through
	the last element of the receiver.  Use #atEndOfFile to determine end
	of file status with feof(), required for reliable end of file test on
	OS pipes."

	| newStream buffer |
	buffer _ buffer1 species new: 1000.
	newStream _ WriteStream on: (buffer1 species new: 100).
	[self atEndOfFile] whileFalse: [newStream nextPutAll: (self nextInto: buffer)].
	^ newStream contents! !

!AttachableFileStream methodsFor: 'testing' stamp: 'dtl 3/26/2006 15:52'!
isPipe

	^ false
! !

!AttachableFileStream methodsFor: 'attaching' stamp: 'dtl 9/12/2023 18:55'!
name: aSymbolOrString attachTo: anIOHandle writable: readWriteFlag
	"Attach to an existing file handle, assumed to have been previously opened by the underlying operating system."

	name _ aSymbolOrString.
	fileID _ anIOHandle.
	readWriteFlag ifTrue: [self readWrite] ifFalse: [self readOnly].
	self useEncoding.
	self register.
! !

!AttachableFileStream methodsFor: 'attaching' stamp: 'dtl 9/12/2023 19:06'!
useEncoding
	"Adopt the Cuis idiom, falling back on #ascii if not present. A stream that
	is not in binary mode is assumed to be using a character encoder. Traditionally
	this simply referred to eight bit characters as opposed to uninterpreted bytes."

	^ (self respondsTo: #useCharacters)
		ifTrue: [ self perform: #useCharacters ]
		ifFalse: [self perform: #ascii ]
! !

!AttachableFileStream methodsFor: 'nonblocking' stamp: 'dtl 8/20/2022 00:36'!
setBlocking

	OSProcess accessor setBlocking: fileID! !

!AttachableFileStream methodsFor: 'nonblocking' stamp: 'dtl 8/20/2022 00:36'!
setNonBlocking

	OSProcess accessor setNonBlocking: fileID! !

!AttachableFileStream methodsFor: 'deprecated' stamp: 'dtl 8/20/2022 00:53'!
ioHandle

	"Deprecated, retained for support of older versions of CommandShell (19-Aug-2022)"

	^ fileID! !

!AttachableFileStream class methodsFor: 'file creation' stamp: 'dtl 6/12/1999 15:53'!
fileNamed: fileName

	self shouldNotImplement
! !

!AttachableFileStream class methodsFor: 'file creation' stamp: 'dtl 6/12/1999 15:53'!
isAFileNamed: fileName

	self shouldNotImplement
! !

!AttachableFileStream class methodsFor: 'file creation' stamp: 'dtl 6/12/1999 15:54'!
newFileNamed: fileName

	self shouldNotImplement
! !

!AttachableFileStream class methodsFor: 'file creation' stamp: 'dtl 6/12/1999 15:55'!
oldFileNamed: fileName

	self shouldNotImplement
! !

!AttachableFileStream class methodsFor: 'file creation' stamp: 'dtl 6/12/1999 15:56'!
readOnlyFileNamed: fileName

	self shouldNotImplement
! !

!AttachableFileStream class methodsFor: 'initialize-release' stamp: 'dtl 10/19/2001 21:53'!
initialize

	"AttachableFileStream initialize"

	UseIOHandle _ (Smalltalk hasClassNamed: #IOHandle)
! !

!AttachableFileStream class methodsFor: 'instance creation' stamp: 'dtl 3/15/2006 19:50'!
name: aSymbolOrString attachTo: anIOHandle writable: readWriteFlag 
	"Create a new instance attached to anIOHandle, where anIOHandle
	represents an open IO channel. For write streams, this represents two
	Smalltalk streams which write to the same OS file or output stream,
	presumably with interleaved output. The purpose of this method is to
	permit a FileStream to be attached to an existing IOHandle, such as
	the IOHandle for standard input, standard output, and standard error."

	^ (super basicNew
		name: aSymbolOrString
		attachTo: anIOHandle
		writable: readWriteFlag) initialize! !

!AttachableFileStream class methodsFor: 'registry' stamp: 'dtl 8/25/2006 18:15'!
register: anObject
	"An attachable file stream is generally either a second reference to an
	existing file stream, or a reference to a transient object such as a pipe
	endpoint. There is no need to register it for finalization."

	^ anObject! !

!AttachableFileStream class methodsFor: 'registry' stamp: 'dtl 8/25/2006 18:15'!
unregister: anObject
	"An attachable file stream is generally either a second reference to an
	existing file stream, or a reference to a transient object such as a pipe
	endpoint. There is no need to register it for finalization."

	^ anObject! !

!AttachableFileStream class methodsFor: 'examples' stamp: 'dtl 8/20/2022 00:37'!
stdIn

	"self stdIn"

	^ self name: 'stdIn' attachTo: OSProcess thisOSProcess stdIn fileID writable: false
! !

!AsyncFileReadStream methodsFor: 'finalization' stamp: 'dtl 6/17/2006 08:08'!
actAsExecutor

	super actAsExecutor.
	eventHandler _ nil! !

!AsyncFileReadStream methodsFor: 'finalization' stamp: 'dtl 9/3/2010 16:57'!
finalize
	"Shut the the event handler before closing the file, otherwise aio
	functions may reference a closed file handle."

	eventHandler ifNotNil: [eventHandler finalize].
	super finalize
! !

!AsyncFileReadStream methodsFor: 'converting' stamp: 'dtl 7/8/2003 20:53'!
asAsyncFileReadStream

	^ self
! !

!AsyncFileReadStream methodsFor: 'converting' stamp: 'dtl 8/20/2022 01:00'!
asAttachableFileStream
	"Answer a replacement for this object, with no asynchronous event handling.
	Do not close the fileID when this object is finalized."

	self keepOpen.
	^ AttachableFileStream
		name: self name
		attachTo: fileID
		writable: self isReadOnly not
! !

!AsyncFileReadStream methodsFor: 'converting' stamp: 'dtl 8/20/2022 00:33'!
asBufferedAsyncFileReadStream
	"Answer a replacement for this object, with buffered output."

	^ BufferedAsyncFileReadStream
		name: self name
		attachTo: fileID
		writable: self isReadOnly not
! !

!AsyncFileReadStream methodsFor: 'initialize-release' stamp: 'dtl 7/8/2003 21:48'!
close

	self disableEventHandling.
	^ super close! !

!AsyncFileReadStream methodsFor: 'initialize-release' stamp: 'dtl 8/20/2022 00:33'!
initialize

	super initialize.
	self readOnly.
	OSProcess accessor setNonBlocking: fileID.
	self enableEventHandling
! !

!AsyncFileReadStream methodsFor: 'initialize-release' stamp: 'dtl 7/8/2003 21:48'!
open

	^ super open initialize
! !

!AsyncFileReadStream methodsFor: 'read event handling' stamp: 'dtl 9/3/2010 16:50'!
disableEventHandling

	eventHandler
		ifNotNil: [eventHandler removeDependent: self; close].
	self eventHandler: nil! !

!AsyncFileReadStream methodsFor: 'read event handling' stamp: 'dtl 11/25/2006 10:56'!
enableEventHandling

	self eventHandler: (AioEventHandler aioPluginPresent
		ifTrue: [AioEventHandler
				forFileStream: self
				exceptions: true
				readEvents: true
				writeEvents: false]
		ifFalse: [PseudoAioEventHandler new]).
	self eventHandler addDependent: self! !

!AsyncFileReadStream methodsFor: 'read event handling' stamp: 'dtl 7/8/2003 19:29'!
hasValidHandler

	^ self eventHandler notNil and: [eventHandler hasValidHandler]
! !

!AsyncFileReadStream methodsFor: 'accessing' stamp: 'dtl 7/8/2003 19:26'!
eventHandler
	"The aio event handler. Provides notification whenever external data is available."

	^ eventHandler! !

!AsyncFileReadStream methodsFor: 'accessing' stamp: 'dtl 7/8/2003 19:26'!
eventHandler: anAioEventHandler
	"The aio event handler. Provides notification whenever external data is available."

	eventHandler _ anAioEventHandler! !

!AsyncFileReadStream methodsFor: 'read, write, position' stamp: 'dtl 9/2/2006 09:58'!
upToEndOfFile
	"Answer a subcollection from the current access position through
	the last element of the receiver.  Use #atEndOfFile to determine end
	of file status with feof(), required for reliable end of file test on
	OS pipes. Close the file when end of file is detected."

	| b |
	b _ super upToEndOfFile.
	self isReadOnly
		ifTrue: [self close].
	^ b! !

!AsyncFileReadStream methodsFor: 'updating' stamp: 'dtl 7/8/2003 21:22'!
update: aParameter

	(aParameter == self eventHandler)
		ifTrue: [self changed]
! !

!AsyncFileReadStream class methodsFor: 'examples' stamp: 'dtl 7/9/2003 20:16'!
stdIn

	"self stdIn"

	^ super stdIn initialize
! !

!BufferedAsyncFileReadStream methodsFor: 'finalization' stamp: 'dtl 6/17/2006 08:10'!
actAsExecutor

	super actAsExecutor.
	nonBlockingMode _ nil.
	readBuffer _ nil.
	readSyncSemaphore _ nil.
	dataAvailableSemaphore _ nil
! !

!BufferedAsyncFileReadStream methodsFor: 'read ahead buffer' stamp: 'dtl 7/30/2023 15:54'!
appendAllToBuffer: chars
	"Append all chars to readBuffer, then signal dataAvailableSemaphore to inform
	any blocked reader that the read can proceed. Also generate a #changed event
	to inform any interested objects that new data has become available."

	self readSyncSemaphore critical:
		[ | pos |
		pos _ self readBuffer position.
		readBuffer setToEnd.
		readBuffer nextPutAll: chars.
		readBuffer position: pos].
	self dataAvailableSemaphore signal.
	self changed! !

!BufferedAsyncFileReadStream methodsFor: 'read ahead buffer' stamp: 'dtl 10/1/2006 17:12'!
appendToBuffer: aCharacter
	"Append aCharacter to readBuffer, then signal dataAvailableSemaphore to inform
	any blocked reader that the read can proceed, and trigger a #dataReady event to
	inform any interested objects that new data has become available."

	| pos |
	self readSyncSemaphore critical:
		[pos _ self readBuffer position.
		readBuffer setToEnd.
		readBuffer nextPut: aCharacter.
		readBuffer position: pos].
	self dataAvailableSemaphore signal.
	self changed! !

!BufferedAsyncFileReadStream methodsFor: 'read ahead buffer' stamp: 'dtl 7/30/2023 14:09'!
moveAvailableDataFrom: sqFile

	| count bufferSize buffer |
	[ readBuffer notNil and: [readBuffer size > self readBufferMemoryWarningThreshold] ]
		whileTrue: [ (Delay forSeconds: 2) wait ]. "Spin wait until space becomes available"
	bufferSize _ self readAheadChunkSize.
	buffer _ String new: bufferSize.
	count _ self readAvailableDataFrom: sqFile into: buffer.
	count > 0
		ifTrue:
			[count == bufferSize
				ifTrue:
					[self appendAllToBuffer: buffer]
				ifFalse:
					[self appendAllToBuffer: (buffer copyFrom: 1 to: count)]]
		ifFalse:
			[(self closed or: [OSProcess accessor isAtEndOfFile: fileID])
				ifTrue:
					[self disableEventHandling]]
! !

!BufferedAsyncFileReadStream methodsFor: 'read ahead buffer' stamp: 'dtl 8/20/2022 00:38'!
moveAvailableDataToBuffer

	^ self moveAvailableDataFrom: fileID.
! !

!BufferedAsyncFileReadStream methodsFor: 'read ahead buffer' stamp: 'dtl 3/4/2017 14:06'!
readAvailableDataFrom: sqFile into: buffer
	"Some implementations of the read primitive may read character by character
	from standard input. Handle that case by looping while data available. Answer
	the number of bytes read into the buffer."

	| bytesRead |
	bytesRead _ 0.
	[ | count |
		[count _ self primRead: sqFile
				into: buffer
				startingAt: 1 + bytesRead
				count: self readAheadChunkSize - bytesRead]
			on: Error	"Could fail if closed"
			do: [count _ 0].
	bytesRead _ bytesRead + count.
	count > 0] whileTrue.
	^ bytesRead.
! !

!BufferedAsyncFileReadStream methodsFor: 'read ahead buffer' stamp: 'dtl 7/6/2003 10:15'!
waitForBufferAvailable
	"Block if the readBuffer has grown too large. No-op for now, but add
	this later if large pipes prove to be a problem."! !

!BufferedAsyncFileReadStream methodsFor: 'read ahead buffer' stamp: 'dtl 7/6/2003 10:15'!
waitForDataReady
	"Block until at least one character is available in the readBuffer. This is not
	thread safe, and only one Process is permitted to send this message."

	self dataAvailableSemaphore wait
! !

!BufferedAsyncFileReadStream methodsFor: 'read ahead buffer' stamp: 'dtl 2/22/2007 08:09'!
waitForDataReady: count 
	"Block until at least count characters are available in the readBuffer"

	[self readSyncSemaphore
		critical: [self readBuffer size - readBuffer position < count]]
		whileTrue: [self waitForDataReady]! !

!BufferedAsyncFileReadStream methodsFor: 'converting' stamp: 'dtl 8/20/2022 00:37'!
asAsyncFileReadStream
	"Answer a replacement for this object, with asynchronous event handling but
	no buffering."

	^ AsyncFileReadStream
		name: self name
		attachTo: fileID
		writable: self isReadOnly not
! !

!BufferedAsyncFileReadStream methodsFor: 'converting' stamp: 'dtl 7/8/2003 20:55'!
asBufferedAsyncFileReadStream

	^ self! !

!BufferedAsyncFileReadStream methodsFor: 'testing' stamp: 'dtl 2/22/2007 08:00'!
atEnd
	"Answer whether the receiver can access any more objects.

	Warning: If this instance represents the reader end of an OS pipe, it
	is possible for the #atEnd test to give a false negative. In particular, after
	closing the writer end of an empty OSPipe, the reader may not appear to
	be atEnd until some time has elapsed, or until an explicit read on the pipe
	causes the status to be updated. To illustrate the problem:

		(OSPipe new setBufferedReader; yourself) closeWriter; atEnd>>false
		(OSPipe new ) closeWriter; next; yourself; atEnd>>true
		(OSPipe new setBufferedReader; yourself) closeWriter; next; yourself; atEnd>>true"

	^ self readSyncSemaphore
		critical: [self readBuffer atEnd
				and: [super atEnd]]! !

!BufferedAsyncFileReadStream methodsFor: 'testing' stamp: 'dtl 2/22/2007 08:01'!
atEndOfFile
	"Answer whether the receiver is at its end based on the result of
	the last read operation. This uses feof() to test the underlying file
	stream status, and can be used as an alternative to #atEnd, which
	does not properly report end of file status for an OSPipe."

	^ self readSyncSemaphore
		critical: [self readBuffer atEnd
				and: [fileID isNil
						or: [OSProcess accessor isAtEndOfFile: fileID]]]! !

!BufferedAsyncFileReadStream methodsFor: 'testing' stamp: 'dtl 7/9/2003 20:59'!
isBlocking
	"Answer true if set to blocking mode."

	^ self nonBlockingMode not! !

!BufferedAsyncFileReadStream methodsFor: 'read, write, position' stamp: 'dtl 2/22/2007 08:02'!
basicNext
	"Answer the next byte from this file, or nil if at the end of the file.
	If the readBuffer is empty, force a basicNext in order to ensure that
	the end of file flag is updated (in stdio stream)."

	(self readSyncSemaphore
			critical: [self readBuffer atEnd])
		ifTrue: [^ super basicNext]
		ifFalse: [[self isBlocking]
				ifTrue: [self waitForDataReady].
			^ self readSyncSemaphore
				critical: [readBuffer next]]! !

!BufferedAsyncFileReadStream methodsFor: 'read, write, position' stamp: 'dtl 7/30/2023 21:35'!
next

	((self readSyncSemaphore
				critical: [self readBuffer atEnd])
			and: [self isBlocking])
		ifTrue: [self waitForDataReady].
	^ self readSyncSemaphore
		critical: [readBuffer next ifNotNil: [ :char | readBuffer pruneCollection. char]]! !

!BufferedAsyncFileReadStream methodsFor: 'read, write, position' stamp: 'dtl 7/30/2023 21:02'!
next: n into: aString startingAt: startIndex
	"Read n bytes into the given string.
	Return aString or a partial copy if less than n elements have been read."

	| count |
	count _ self readInto: aString startingAt: startIndex count: n.
	count > 0 ifTrue: [ self readBuffer pruneCollection ].
	count = n
		ifTrue: [^ aString]
		ifFalse: [^ aString copyFrom: 1 to: startIndex+count-1]! !

!BufferedAsyncFileReadStream methodsFor: 'read, write, position' stamp: 'dtl 9/3/2010 17:01'!
peek
	"Answer what would be returned if the message next were sent to the
	receiver. If the receiver is at the end, answer nil. "

	((self readSyncSemaphore
				critical: [self readBuffer atEnd])
			and: [self isBlocking])
		ifTrue: [self waitForDataReady].
	^ self readSyncSemaphore
		critical: [readBuffer peek]! !

!BufferedAsyncFileReadStream methodsFor: 'read, write, position' stamp: 'dtl 3/21/2007 21:56'!
readInto: byteArray startingAt: startIndex count: count 
	"Read into the given array as specified, and return the count actually
	transferred. "

	| s size |
	self isBlocking
		ifTrue: [self waitForDataReady: count.
			s _ self readSyncSemaphore
						critical: [self readBuffer next: count].
			size _ count]
		ifFalse: [size _ self readBuffer size - readBuffer position min: count.
			s _ self readSyncSemaphore
						critical: [self readBuffer next: size]].
	byteArray
		replaceFrom: startIndex
		to: startIndex + size - 1
		with: s.
	^ size! !

!BufferedAsyncFileReadStream methodsFor: 'read, write, position' stamp: 'dtl 3/19/2007 18:28'!
upTo: delim 

	^ self readSyncSemaphore critical: [self readBuffer upTo: delim]
! !

!BufferedAsyncFileReadStream methodsFor: 'read, write, position' stamp: 'dtl 12/21/2007 13:14'!
upToEndOfFile
	"Answer a subcollection from the current access position through
	the last element of the receiver.  Use #atEndOfFile to determine end
	of file status with feof(), required for reliable end of file test on
	OS pipes. Close the file when end of file is detected."

	| newStream buffer |
	buffer _ buffer1 species new: 1000.
	newStream _ WriteStream on: (buffer1 species new: 100).
	[self atEndOfFile] whileFalse:
		[self moveAvailableDataToBuffer.
		newStream nextPutAll: (self nextInto: buffer)].
	self isReadOnly ifTrue: [self close].
	^ newStream contents
! !

!BufferedAsyncFileReadStream methodsFor: 'accessing' stamp: 'dtl 7/6/2003 10:15'!
dataAvailableSemaphore
	"Signalled one or more times when data becomes available. Only one Process
	is permitted to wait on this Semaphore."

	^ dataAvailableSemaphore ifNil: [dataAvailableSemaphore _ Semaphore new]
! !

!BufferedAsyncFileReadStream methodsFor: 'accessing' stamp: 'dtl 7/9/2003 20:57'!
nonBlockingMode
	"True if nonblocking read behavior should be implemented"

	^ nonBlockingMode ifNil: [nonBlockingMode _ true]
! !

!BufferedAsyncFileReadStream methodsFor: 'accessing' stamp: 'dtl 7/9/2003 20:58'!
nonBlockingMode: trueOrFalse
	"True if nonblocking read behavior should be implemented"

	nonBlockingMode _ trueOrFalse
! !

!BufferedAsyncFileReadStream methodsFor: 'accessing' stamp: 'dtl 7/30/2023 20:53'!
readBuffer
	"Read ahead buffer, filled asynchronously as data becomes available on the IO channel"

	^ readBuffer
		ifNil: [readBuffer _ (ResettableBufferStream with: String new)
						resetLimit: 20000]! !

!BufferedAsyncFileReadStream methodsFor: 'accessing' stamp: 'dtl 7/6/2003 10:15'!
readBuffer: aStream

	readBuffer _ aStream! !

!BufferedAsyncFileReadStream methodsFor: 'accessing' stamp: 'dtl 7/6/2003 10:15'!
readSyncSemaphore
	"A semaphore for synchronizing access to readBuffer"

	^ readSyncSemaphore ifNil: [readSyncSemaphore _ Semaphore forMutualExclusion]! !

!BufferedAsyncFileReadStream methodsFor: 'defaults' stamp: 'dtl 7/6/2003 10:15'!
maxReadBufferSize
	"Replace readBuffer when position exceeds this."

	^ 40000
! !

!BufferedAsyncFileReadStream methodsFor: 'defaults' stamp: 'dtl 7/6/2003 10:15'!
readAheadChunkSize
	"The async read ahead process will read at most this many characters.
	Notes: On my system (dtl), a chunk size of 2000 leads to ExternalCommandShell
	overdriving the stdout stream when doing (for example) a long directory listing.
	I have added error handling to accommodate this, but I do know know how
	reliable it is, so I would prefer to avoid generating the condition. A chunk
	size of 200 is small enough that performance is noticably impacted in a
	CommandShell window. A chunk size of 1000 seems to produce good overall
	behavior."

	^ 1000! !

!BufferedAsyncFileReadStream methodsFor: 'defaults' stamp: 'dtl 12/7/2012 19:54'!
readBufferMemoryWarningThreshold
	"A read buffer larger that this may start causing problems. Issue a warning
	before the object memory becomes too large." 

	^self maxReadBufferSize * 1000
! !

!BufferedAsyncFileReadStream methodsFor: 'initialize-release' stamp: 'dtl 7/9/2003 20:54'!
setBlocking
	"Set for blocking reads. Default is nonblocking mode."

	self nonBlockingMode: false
! !

!BufferedAsyncFileReadStream methodsFor: 'initialize-release' stamp: 'dtl 7/9/2003 20:55'!
setNonBlocking
	"Set for nonblocking reads. This is the default mode."

	self nonBlockingMode: true
! !

!BufferedAsyncFileReadStream methodsFor: 'updating' stamp: 'dtl 7/8/2003 22:06'!
update: aParameter
	"A #changed event is generated as a side effect of this method"

	(aParameter == self eventHandler)
		ifTrue: [self moveAvailableDataToBuffer]
! !

!BufferedAsyncFileReadStream class methodsFor: 'examples' stamp: 'dtl 7/9/2003 20:24'!
stdIn

	"self stdIn"

	^ super stdIn
! !

!ResettableBufferStream methodsFor: 'collections - accessing' stamp: 'dtl 7/30/2023 21:06'!
pruneCollection
	"Assume that position refers to the stream position of elements most recently
	read from the stream, and that there wil be no further need to read those elements.
	Remove collection elements prior to position to maintain a small collection size."

	position > self resetLimit
		ifTrue: [ self removePreviouslyRead ]
! !

!ResettableBufferStream methodsFor: 'collections - accessing' stamp: 'dtl 7/23/2023 11:51'!
removePreviouslyRead

	| newReadLimit newWriteLimit |
	newReadLimit _ readLimit - position .
	newWriteLimit _ writeLimit - position.
	collection _ collection allButFirst: position..
	readLimit _ newReadLimit.
	writeLimit _ newWriteLimit.
	position _ 0.
! !

!ResettableBufferStream methodsFor: 'accessing' stamp: 'dtl 7/30/2023 13:03'!
resetLimit
	"Previously read entries will be discarded from the collection when this limit is exceeded"

	^ resetLimit ifNil: [ resetLimit _ 20000 ]
! !

!ResettableBufferStream methodsFor: 'accessing' stamp: 'dtl 7/30/2023 12:41'!
resetLimit: count

	resetLimit _ count

! !

!ResettableBufferStream methodsFor: 'accessing' stamp: 'dtl 1/2/2024 09:27:43'!
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."
	
	"Copied from Squeak PositionableStream>>upToEnd which is not currently implemented in Cuis"

	| newStream |
	newStream := WriteStream on: (self collectionSpecies new: 100).
	[self atEnd] whileFalse: [ newStream nextPut: self next ].
	^ newStream contents! !

!ExternalPipe methodsFor: 'printing' stamp: 'dtl 4/2/2006 11:40'!
printOn: aStream
	"The implementation of Stream>>printOn: has bad side effects when used
	for OSPipe. This implementation is copied from Object."

	| title |
	title _ self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title! !

!ExternalPipe methodsFor: 'testing' stamp: 'dtl 4/2/2006 21:14'!
atEnd
	"Answer whether the receiver can access any more objects."

	^ writer closed and: [self peek == nil]
! !

!ExternalPipe methodsFor: 'testing' stamp: 'dtl 6/4/2006 16:01'!
atEndOfFile
	"Answer whether the receiver is at its end based on the result of
	the last read operation. This uses feof() to test the underlying file
	stream status, and can be used as an alternative to #atEnd, which
	does not properly report end of file status for an OSPipe."

	^ reader atEndOfFile
! !

!ExternalPipe methodsFor: 'testing' stamp: 'dtl 9/16/2002 17:35'!
closed

	^ reader closed! !

!ExternalPipe methodsFor: 'testing' stamp: 'dtl 3/26/2006 15:48'!
isPipe

	^ true
! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 3/7/2006 20:00'!
blocking
	"True if reader end is set to blocking mode."

	^ blocking ifNil: [blocking _ true]! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 3/7/2006 20:00'!
blocking: trueOrFalse
	"True if reader end is set to blocking mode."

	blocking _ trueOrFalse! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 4/16/2003 06:01'!
contents
	"Answer contents of the pipe, and return the contents to the pipe so it can still be read."

	"ExternalPipe new nextPutAll: 'hello'; contents"

	| s |
	self closed ifTrue:
		[self notify: self printString, ' ', self reader printString, ' closed'.
		^ nil].
	s _ self reader upToEnd.
	s isEmpty ifFalse:
		[self writer closed
			ifTrue: [self notify: self printString, ' ', self writer printString,
								' closed, cannot replace contents']
			ifFalse: [self nextPutAll: s]].
	^ s! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 4/12/2014 16:04'!
flush
	^writer flush! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 9/16/2002 17:33'!
next
	"Answer the next object accessible by the receiver."

	^ reader next! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 9/16/2002 17:33'!
next: anInteger 
	"Answer the next anInteger elements of my collection."

	^ reader next: anInteger
! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 3/7/2006 19:44'!
nextPut: anObject 
	"Insert the argument, anObject, as the next object accessible by the 
	receiver. Answer anObject."

	^ writer nextPut: anObject! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 3/7/2006 19:44'!
nextPutAll: aCollection 
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	^ writer nextPutAll: aCollection! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 9/16/2002 17:34'!
peek

	^ reader peek! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 3/7/2006 19:44'!
reader
	"Answer a stream on the read end of the pipe."

	^ reader! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 3/7/2006 19:44'!
reader: aReadStream

	reader _ aReadStream! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 9/18/2002 20:29'!
upToEnd
	"Answer the remaining elements in the string"

	reader closed
		ifTrue: [^ '']
		ifFalse: [^ reader upToEnd]! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 11/11/2011 09:27'!
upToEndOfFile
	"Answer the remaining elements in the pipe.  Use #isAtEndOfFile: to
	determine end of file status with feof(), required for reliable end of
	file test on OS pipes."

	reader closed
		ifTrue: [^ '']
		ifFalse: [^ reader upToEndOfFile]! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 3/7/2006 19:44'!
writer
	"Answer a stream on the write end of the pipe."

	^ writer! !

!ExternalPipe methodsFor: 'accessing' stamp: 'dtl 3/7/2006 19:44'!
writer: aWriteStream

	writer _ aWriteStream! !

!ExternalPipe methodsFor: 'finalization' stamp: 'dtl 3/7/2006 19:44'!
close

	self closeWriter; closeReader
! !

!ExternalPipe methodsFor: 'finalization' stamp: 'dtl 3/7/2006 19:44'!
closeReader

	reader ifNotNil: [reader close]
! !

!ExternalPipe methodsFor: 'finalization' stamp: 'dtl 3/7/2006 19:44'!
closeWriter

	writer ifNotNil: [writer close]
! !

!ExternalPipe methodsFor: 'character writing' stamp: 'dtl 3/7/2006 19:44'!
cr
	"Append a return character to the receiver."

	self writer cr! !

!ExternalPipe methodsFor: 'initialize-release' stamp: 'dtl 3/25/2006 14:08'!
initialize

	^ self makePipe
! !

!ExternalPipe methodsFor: 'initialize-release' stamp: 'dtl 8/19/2022 19:35'!
makePipe
	"Create an OS pipe and attach it to my input and output streams."

	| handleArray |
	handleArray _ OSProcess accessor primCreatePipe.
	handleArray isNil
		ifTrue:
			[self error: 'cannot create OS pipe']
		ifFalse:
			[self reader: (AttachableFileStream
							name: 'pipeReader'
							attachTo: (handleArray at: 1)
							writable: false).
			self writer: (AttachableFileStream
							name: 'pipeWriter'
							attachTo: (handleArray at: 2)
							writable: true)]
! !

!ExternalPipe methodsFor: 'initialize-release' stamp: 'dtl 8/20/2022 00:38'!
setBlocking
	"Set the reader side of the pipe for blocking reads."

	reader ifNotNil: [OSProcess accessor setBlocking: reader fileID].
	self blocking: true
! !

!ExternalPipe methodsFor: 'initialize-release' stamp: 'dtl 4/2/2006 19:06'!
setBufferedReader
	"Use an event driven AsyncFileReadStream to represent the reader end of the pipe.
	This should be used if the pipe will be read by a Smalltalk process. It should not
	be used if the pipe is to be read by an external OS process."

	reader ifNotNil:
		[reader removeDependent: self.
		reader unregister.
		self reader: reader asBufferedAsyncFileReadStream.
		self setNonBlocking.
		reader addDependent: self.
		^ true].
	^ false
! !

!ExternalPipe methodsFor: 'initialize-release' stamp: 'dtl 8/20/2022 00:38'!
setNonBlocking
	"Set the reader side of the pipe for non-blocking reads."

	reader ifNotNil: [OSProcess accessor setNonBlocking: reader fileID].
	self blocking: false
! !

!ExternalPipe methodsFor: 'updating' stamp: 'dtl 3/15/2006 07:17'!
triggerDataReady
	"Notify any object waiting for data ready on the pipe."

	self triggerEvent: #dataReady.
! !

!ExternalPipe methodsFor: 'updating' stamp: 'dtl 1/18/2003 14:31'!
update: aParameter
	"Notify any object waiting for data ready on the pipe."

	self changed.
	self triggerDataReady.
	^ super update: aParameter
! !

!ExternalPipe class methodsFor: 'instance creation' stamp: 'dtl 4/2/2006 21:34'!
blockingPipe
	"Warning: a read on a blocking pipe will hang the VM if there is insufficient
	data in the pipe to fulfill the read request."

	"OSPipe blockingPipe"

	^ super basicNew initialize; setBlocking
! !

!ExternalPipe class methodsFor: 'instance creation' stamp: 'dtl 4/2/2006 21:34'!
bufferedBlockingPipe
	"Warning: a read on a blocking pipe will hang the VM if there is insufficient
	data in the pipe to fulfill the read request."

	"OSPipe bufferedBlockingPipe"

	^ (super basicNew initialize; setBlocking) setBufferedReader; yourself
! !

!ExternalPipe class methodsFor: 'instance creation' stamp: 'dtl 4/2/2006 21:33'!
bufferedNonBlockingPipe

	"OSPipe bufferedNonBlockingPipe"

	^ (super basicNew initialize; setNonBlocking) setBufferedReader; yourself
! !

!ExternalPipe class methodsFor: 'instance creation' stamp: 'dtl 4/2/2006 21:35'!
new

	"ExternalPipe new"

	^ self nonBlockingPipe
! !

!ExternalPipe class methodsFor: 'instance creation' stamp: 'dtl 3/25/2006 14:14'!
nonBlockingPipe

	"OSPipe nonBlockingPipe"

	^ super basicNew initialize; setNonBlocking
! !

!ExternalPipe class methodsFor: 'examples' stamp: 'dtl 3/7/2006 19:44'!
testPipe
	"OSPipe testPipe inspect"

	| pipe result |
	pipe _ self new.
	pipe nextPutAll: 'string to send through an OSPipe'.
	pipe writer close.
	result _ pipe upToEnd.
	pipe close.
	^ result
! !

!OSPipe methodsFor: 'accessing' stamp: 'dtl 10/14/2001 12:16'!
next
	"Answer the next object accessible by the receiver."

	| c |
	nextChar isNil
		ifTrue:
			[^ [reader next]
				on: Error
				do: [nil]]
		ifFalse:
			[c _ nextChar.
			nextChar _ nil.
			^ c]
! !

!OSPipe methodsFor: 'accessing' stamp: 'dtl 2/24/2013 10:24'!
next: anInteger 
	"Answer the next anInteger elements of my collection."

	| c strm |
	strm _ WriteStream on: String new.
	(1 to: anInteger) do:
		[:index |
		c _ self next.
		c isNil
			ifTrue: [^ strm contents]	
			ifFalse: [strm nextPut: c. false]].
	^ strm contents
! !

!OSPipe methodsFor: 'accessing' stamp: 'dtl 5/16/2006 06:52'!
peek

	^ nextChar isNil
		ifTrue: [reader closed
				ifFalse: [nextChar _ reader next]]
		ifFalse: [nextChar]! !

!OSPipe methodsFor: 'accessing' stamp: 'dtl 2/24/2013 10:24'!
upToEnd
	"Answer the remaining elements in the string. This method is retained for backward
	compatibility with older versions of CommandShell."

	| strm s |
	strm _ WriteStream on: String new.
	[(s _ self next: 2000) isEmpty
		ifTrue: [^ strm contents]
		ifFalse: [strm nextPutAll: s]] repeat
! !

!OSPipe methodsFor: 'accessing' stamp: 'dtl 2/24/2013 10:24'!
upToEndOfFile
	"Answer the remaining elements in the pipe. Use #atEndOfFile to
	determine end of file status with feof(), required for reliable end of
	file test on OS pipes. Compare #upToEnd, which uses the generic end
	of file test in FilePlugin."

	| strm d s |
	strm _ WriteStream on: String new.
	d _ Delay forMilliseconds: 200.
	[(s _ self next: 2000) isEmpty
		ifTrue: [self atEndOfFile
			ifTrue: [^ strm contents]
			ifFalse: [d wait]]
		ifFalse: [strm nextPutAll: s]] repeat
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'accessing' stamp: 'dtl 4/30/2006 13:47'!
delay
	"Answer the value of delay"

	^ delay! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'remote image commands' stamp: 'dtl 12/15/2013 15:50'!
doRemote: remoteBlock doLocal: localBlock
	"Evaluate remoteBlock in a remote Squeak image. While the remote Squeak is
	still running, evaluate localBlock. When done, terminate the remote Squeak
	image and answer the result of evaluating localBlock."

	"self new
		doRemote: [OSProcess thisOSProcess stdOut nextPutAll: 'hello from child Squeak';
						nextPut: Character lf]
		doLocal: [(Delay forSeconds: 1) wait.
						OSProcess thisOSProcess stdOut nextPutAll: 'hello from parent Squeak';
					nextPut: Character lf]"

	| result |
	self shouldnt: [remoteProcess _ OSProcess thisOSProcess forkHeadlessSqueak]
		raise: Warning.
	self assert: remoteProcess notNil. "Fail if fork did not succeed"
	(remoteProcess == OSProcess thisOSProcess)
		ifTrue:
			["remote child Squeak"
			remoteBlock value]
		ifFalse:
			["parent Squeak"
			[[remoteProcess isRunning] whileFalse: [self delay wait].
			self delay wait.
			result _ localBlock value]
				ensure:	[remoteProcess terminate.
					[remoteProcess isComplete] whileFalse: [self delay wait].
					self delay wait.
					result]].
	^ result
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'remote image commands' stamp: 'dtl 4/30/2006 13:47'!
quitImage

	fileStream close.
	OSProcess snapshot: false andQuit: true
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'failures' stamp: 'eem 3/27/2014 11:28'!
expectedFailures
	^UnixOSProcessAccessor basicNew safeToForkSqueak
		ifTrue: [#()]
		ifFalse: [#(	testCooperatingProcesses01
					testCooperatingProcesses02
					testCooperatingProcesses03
					testCooperatingProcesses04
					testCooperatingProcesses05
					testFailFileLockOnLockedFile
					testFailLockOnLockedOverlappedRegion
					testFailLockOnLockedRegion
					testFailLockOnLockedSupersetRegion
					testFailRegionLockOnLockedFile
					testLockEntireFileForWrite01
					testLockEntireFileForWrite02
					testLockEntireFileForWrite03
					testLockEntireFileForWrite04
					testLockEntireFileForWrite05
					testLockEntireFileForWrite06
					testLockRegionForRead01
					testLockRegionForRead02
					testLockRegionForWrite01
					testLockRegionForWrite02
					testLockRegionForWrite03
					testLockRegionForWrite04
					testLockRegionForWrite05
					testLockRegionForWrite06
					testLockRegionForWrite07
					testLockRegionForWrite08
					testNoFailLockOnAdjacentLockedRegions
					testNoFailLockOnDifferentLockedRegion)]! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'initialize' stamp: 'dtl 12/17/2016 17:54'!
initialize

	initialCompatibilitySetting
		ifNil: [initialCompatibilitySetting _ OSProcess accessor emulateWin32FileLocking]! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'running' stamp: 'dtl 1/15/2018 21:55:39'!
setUp

	(self respondsTo: #timeout: ) "Recent Squeak images with test case timeout"
		ifTrue: [self perform: #timeout: with: 30].
	delay _ Delay forMilliseconds: 150.
	accessor _ ThisOSProcess accessor.
	OSProcess deleteFileNamed: 'junkfile'.
	fileStream _ OSProcess fileNamed: 'junkfile'.
	1000 timesRepeat: (fileStream nextPutAll: 'hello world ').
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'running' stamp: 'dtl 12/17/2016 17:55'!
tearDown

	| d |
	OSProcessAccessor emulateWin32FileLocking: initialCompatibilitySetting.
	d _ Delay forMilliseconds: 50.
	fileStream close.
	remoteProcess ifNotNil:
		[remoteProcess terminate.
		[remoteProcess isComplete] whileFalse: [d wait].
		remoteProcess _ nil]
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 12/17/2016 16:30'!
testCooperatingProcesses01
	"Two cooperating Squeak processes using file region locking to coordinate their
	writes to a shared file."

	"(self selector: #testCooperatingProcesses01) run"

	| result |
	result _ self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream from: 100 to: 120) notNil]) ifNil: [self quitImage].
			fileStream position: 100.
			fileStream nextPutAll: 'THIS '; flush.
			accessor unlockFile: fileStream from: 100 to: 120.
			delay wait.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			fileStream position: 108.
			fileStream nextPutAll: 'A '; flush.
			accessor unlockFile: fileStream from: 108 to: 120.
			delay wait.
			self quitImage]
		doLocal:
			[self assert: (self tryUntil: [(accessor lockFile: fileStream from: 104 to: 120) notNil]).
			fileStream position: 105.
			fileStream nextPutAll: 'is '; flush.
			accessor unlockFile: fileStream from: 104 to: 120.
			delay wait.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			fileStream position: 110.
			fileStream nextPutAll: 'test '; flush.
			accessor unlockFile: fileStream from: 108 to: 120.
			delay wait].
	self tryUntil: [(accessor lockFile: fileStream from: 100 to: 140) notNil].
	fileStream position: 100.
	result _ fileStream next: 14.
	accessor unlockFile: fileStream from: 100 to: 140.
	self assert: result = 'THIS is A test'
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 12/17/2016 16:31'!
testCooperatingProcesses02
	"Two cooperating Squeak processes using file region locking to coordinate their
	writes to a shared file."

	"(self selector: #testCooperatingProcesses02) run"

	| result |
	result _ self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream from: 100 to: 120) notNil]) ifNil: [self quitImage].
			fileStream position: 100.
			fileStream nextPutAll: 'THIS 11111111111111111111111'; flush.
			accessor unlockFile: fileStream from: 100 to: 120.
			delay wait; wait; wait.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			fileStream position: 108.
			fileStream nextPutAll: 'A 3333333333333333333333333'; flush.
			accessor unlockFile: fileStream from: 108 to: 120.
			self quitImage]
		doLocal:
			[self assert: (self tryUntil: [(accessor lockFile: fileStream from: 104 to: 120) notNil]).
			fileStream position: 105.
			fileStream nextPutAll: 'IS 2222222222222222222222222'; flush.
			accessor unlockFile: fileStream from: 104 to: 120.
			delay wait; wait; wait.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			fileStream position: 110.
			fileStream nextPutAll: 'TEST 4444444444444444444444'; flush.
			accessor unlockFile: fileStream from: 108 to: 120].
	self tryUntil: [(accessor lockFile: fileStream from: 100 to: 140) notNil].
	fileStream position: 100.
	result _ fileStream next: 17.
	accessor unlockFile: fileStream from: 100 to: 140.
	self assert: result = 'THIS IS A TEST 44'
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 12/17/2016 16:32'!
testCooperatingProcesses03
	"Two cooperating Squeak processes using file region locking to coordinate their
	writes to a shared file."

	"(self selector: #testCooperatingProcesses03) run"

	| result |
	result _ self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream from: 100 to: 120) notNil]) ifNil: [self quitImage].
			fileStream position: 100.
			fileStream nextPutAll: 'THIS 11111111111111111111111'; flush.
			accessor unlockFile: fileStream from: 100 to: 120.
			delay wait; wait; wait.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			fileStream position: 108.
			fileStream nextPutAll: 'A 3333333333333333333333333'; flush.
			accessor unlockFile: fileStream from: 108 to: 120.
			self quitImage]
		doLocal:
			[self assert: (self tryUntil: [(accessor lockFile: fileStream from: 104 to: 120) notNil]).
			fileStream position: 105.
			fileStream nextPutAll: 'IS 2222222222222222222222222'; flush.
			accessor unlockFile: fileStream from: 104 to: 120.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			fileStream position: 110.
			fileStream nextPutAll: 'TEST 4444444444444444444444'; flush.
			accessor unlockFile: fileStream from: 108 to: 120].
	self tryUntil: [(accessor lockFile: fileStream from: 100 to: 140) notNil].
	fileStream position: 100.
	result _ fileStream next: 17.
	accessor unlockFile: fileStream from: 100 to: 140.
	self assert: result = 'THIS IS 22TEST 44'
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 12/17/2016 16:32'!
testCooperatingProcesses04
	"Two cooperating Squeak processes using file region locking to coordinate their
	writes to a shared file."

	"(self selector: #testCooperatingProcesses04) run"

	| result |
	result _ self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream from: 100 to: 120) notNil]) ifNil: [self quitImage].
			delay wait.
			fileStream position: 100.
			fileStream nextPutAll: 'THIS 11111111111111111111111'; flush.
			accessor unlockFile: fileStream from: 100 to: 120.
			delay wait.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			delay wait.
			fileStream position: 108.
			fileStream nextPutAll: 'A 3333333333333333333333333'; flush.
			accessor unlockFile: fileStream from: 108 to: 120.
			delay wait.
			self quitImage]
		doLocal:
			[self assert: (self tryUntil: [(accessor lockFile: fileStream from: 104 to: 120) notNil]).
			delay wait.
			fileStream position: 105.
			fileStream nextPutAll: 'IS 2222222222222222222222222'; flush.
			accessor unlockFile: fileStream from: 104 to: 120.
			delay wait.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			fileStream position: 110.
			fileStream nextPutAll: 'TEST 4444444444444444444444; flush'.
			accessor unlockFile: fileStream from: 108 to: 120].
	self tryUntil: [(accessor lockFile: fileStream from: 100 to: 140) notNil].
	fileStream position: 100.
	result _ fileStream next: 17.
	accessor unlockFile: fileStream from: 100 to: 140.
	self assert: result = 'THIS IS A TEST 44'
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 12/17/2016 18:05'!
testCooperatingProcesses05
	"Two cooperating Squeak processes using file region locking to coordinate their
	writes to a shared file."

	"(self selector: #testCooperatingProcesses05) run"

	| result |
	result _ self
		doRemote:
			[OSProcess thisOSProcess stdOut nextPutAll: 'starting remote squeak requesting lock on 100 to 120';
				nextPut: Character lf.
			(self tryUntil: [(accessor lockFile: fileStream from: 100 to: 120) notNil]) ifNil: [self quitImage].
			OSProcess thisOSProcess stdOut nextPutAll: 'remote squeak lock acquired on 100 to 120'; nextPut: Character lf; flush.
			fileStream position: 100.
			fileStream nextPutAll: 'THIS '; flush.
			OSProcess thisOSProcess stdOut nextPutAll: 'remote squeak releasing lock on 100 to 120'; nextPut: Character lf; flush.
			accessor unlockFile: fileStream from: 100 to: 120.
			OSProcess thisOSProcess stdOut nextPutAll: 'remote squeak lock released on 100 to 120'; nextPut: Character lf; flush.
			delay wait.
			OSProcess thisOSProcess stdOut nextPutAll: 'remote squeak requesting lock on 108 to: 120'; nextPut: Character lf; flush.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			OSProcess thisOSProcess stdOut nextPutAll: 'remote squeak lock acquired on 108 to: 120'; nextPut: Character lf; flush.
			fileStream position: 108.
			fileStream nextPutAll: 'A '; flush.
			OSProcess thisOSProcess stdOut nextPutAll: 'remote squeak releasing lock on 108 to: 120'; nextPut: Character lf; flush.
			accessor unlockFile: fileStream from: 108 to: 120.
			OSProcess thisOSProcess stdOut nextPutAll: 'remote squeak lock released on 108 to: 120'; nextPut: Character lf; flush.
			delay wait.
			self quitImage]
		doLocal:
			[OSProcess thisOSProcess stdOut nextPutAll: 'starting local squeak requesting lock on 104 to: 120';
				nextPut: Character lf.
			self assert: (self tryUntil: [(accessor lockFile: fileStream from: 104 to: 120) notNil]).
			OSProcess thisOSProcess stdOut nextPutAll: 'local squeak lock acquired on 104 to: 120'; nextPut: Character lf; flush.
			fileStream position: 105.
			fileStream nextPutAll: 'IS '; flush.
			OSProcess thisOSProcess stdOut nextPutAll: 'local squeak releasing lock on 104 to: 120'; nextPut: Character lf; flush.
			accessor unlockFile: fileStream from: 104 to: 120.
			OSProcess thisOSProcess stdOut nextPutAll: 'local squeak lock released on 104 to: 120'; nextPut: Character lf; flush.
			delay wait.
			OSProcess thisOSProcess stdOut nextPutAll: 'local squeak requesting lock on 108 to: 120'; nextPut: Character lf; flush.
			self tryUntil: [(accessor lockFile: fileStream from: 108 to: 120) notNil].
			OSProcess thisOSProcess stdOut nextPutAll: 'local squeak lock acquired on 108 to: 120'; nextPut: Character lf; flush.
			fileStream position: 110.
			fileStream nextPutAll: 'TEST '; flush.
			OSProcess thisOSProcess stdOut nextPutAll: 'local squeak releasing lock on 108 to: 120'; nextPut: Character lf; flush.
			accessor unlockFile: fileStream from: 108 to: 120.
			OSProcess thisOSProcess stdOut nextPutAll: 'local squeak lock released on 108 to: 120'; nextPut: Character lf; flush.
			delay wait].
	self tryUntil: [(accessor lockFile: fileStream from: 100 to: 140) notNil].
	fileStream position: 100.
	result _ fileStream next: 14.
	accessor unlockFile: fileStream from: 100 to: 140.
	self assert: result = 'THIS IS A TEST'
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/7/2006 22:44'!
testLockEntireFileForWrite01
	"Local lock request should succeed"

	"(self selector: #testLockEntireFileForWrite01) run"

	| result |
	result _ self
		doRemote: [nil]
		doLocal: [accessor lockFile: fileStream].
	self delay wait; wait.
	self assert: (result isKindOf: OSFileLock).
	result _ accessor unlockFile: fileStream.
	self assert: (result isKindOf: OSFileLock)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:40'!
testLockEntireFileForWrite02
	"Local lock request should fail"

	"(self selector: #testLockEntireFileForWrite02) run"

	| result |
	result _ self
		doRemote:
			[accessor lockFile: fileStream]
		doLocal: [accessor lockFile: fileStream].
	self delay wait; wait.
	self assert: result isNil.
	"Region is in the cache as a result of the (failed) lock request. This is legitimate
	because the lock exists in some other image, so we want to mark it as locked
	locally. However, it is not quite correct because the remote lock may pertain to
	a different but overlapping region. Nevertheless, this seems like reasonable
	behavior."
	result _ accessor unlockFile: fileStream.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/7/2006 22:45'!
testLockEntireFileForWrite03
	"Local lock request should succeed"

	"(self selector: #testLockEntireFileForWrite03) run"

	| result |
	result _ self
		doRemote:
			[accessor lockFile: fileStream.
			accessor unlockFile: fileStream]
		doLocal:
			[accessor lockFile: fileStream].
	self delay wait; wait.
	self assert: (result isKindOf: OSFileLock).
	result _ accessor unlockFile: fileStream.
	self assert: (result isKindOf: OSFileLock)! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:48'!
testLockEntireFileForWrite04
	"Local lock should be available"

	"(self selector: #testLockEntireFileForWrite04) run"

	| result |
	result _ self
		doRemote: [nil]
		doLocal: [accessor isLockableFile: fileStream].
	self delay wait; wait.
	self should: result.
	"No lock on the region, so the unlock should fail."
	result _ accessor unlockFile: fileStream.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:50'!
testLockEntireFileForWrite05
	"Local lock should be available"

	"(self selector: #testLockEntireFileForWrite05) run"

	| result |
	result _ self
		doRemote:
			[accessor lockFile: fileStream]
		doLocal: [accessor isLockableFile: fileStream].
	self delay wait; wait.
	self shouldnt: result.
	"No local lock in the cache for this file, so the unlock call will appear to fail.
	Actually, it will have passed the unlock request through to the operating system,
	and it will actually unlock the file."
	result _ accessor unlockFile: fileStream.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:49'!
testLockEntireFileForWrite06
	"Local lock should be available"

	"(self selector: #testLockEntireFileForWrite06) run"

	| result |
	result _ self
		doRemote:
			[accessor lockFile: fileStream.
			accessor unlockFile: fileStream]
		doLocal:
			[accessor isLockableFile: fileStream].
	self delay wait; wait.
	self should: result.
	"No local lock in the cache for this file, so the unlock call will appear to fail.
	Actually, it will have passed the unlock request through to the operating system,
	but it will have no effect because the file was never locked."
	result _ accessor unlockFile: fileStream.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/7/2006 22:46'!
testLockRegionForRead01
	"Read lock, the local lock request should succeed"

	"(self selector: #testLockRegionForRead01) run"

	| result |
	result _ self
		doRemote: [accessor lockFile: fileStream from: 100 to: 199 exclusive: false]
		doLocal: [accessor lockFile: fileStream from: 100 to: 199 exclusive: false].
	self delay wait; wait.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199)).
	result _ accessor unlockFile: fileStream from: 100 to: 199 exclusive: false.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199))! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:53'!
testLockRegionForRead02
	"Read lock, the local lock request should fail when requesting a write lock"

	"(self selector: #testLockRegionForRead02) run"

	| result |
	result _ self
		doRemote: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true]
		doLocal: [accessor lockFile: fileStream from: 100 to: 199 exclusive: false].
	self delay wait; wait.
	self assert: result isNil.
	result _ accessor unlockFile: fileStream from: 100 to: 199.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/7/2006 22:46'!
testLockRegionForWrite01
	"Local lock request should succeed"

	"(self selector: #testLockRegionForWrite01) run"

	| result |
	result _ self
		doRemote: [nil]
		doLocal: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true].
	self delay wait; wait.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199)).
	result _ accessor unlockFile: fileStream from: 100 to: 199.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199))! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 4/30/2006 13:47'!
testLockRegionForWrite02
	"Write lock, the local lock request should fail"

	"(self selector: #testLockRegionForWrite02) run"

	| result |
	result _ self
		doRemote: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true]
		doLocal: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true].
	self delay wait; wait.
	self assert: result isNil.
	"Lock is not in the local cache, so unlock will appear to fail. Actually, the region
	will be unlocked."
	accessor unlockFile: fileStream from: 100 to: 199.
	self assert: result isNil.
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:55'!
testLockRegionForWrite03
	"Write lock, nonoverlapping regions, the lock check should succeed"

	"(self selector: #testLockRegionForWrite03) run"

	| result |
	result _ self
		doRemote: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true]
		doLocal: [accessor isLockableFile: fileStream from: 200 to: 299 exclusive: true].
	self delay wait; wait.
	self assert: result.
	result _ accessor unlockFile: fileStream from: 200 to: 299.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:53'!
testLockRegionForWrite04
	"Write lock, overlapping regions, the local lock request should fail"

	"(self selector: #testLockRegionForWrite04) run"

	| result |
	result _ self
		doRemote: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true]
		doLocal: [accessor lockFile: fileStream from: 150 to: 249 exclusive: true].
	self delay wait; wait.
	self assert: result isNil.
	result _ accessor unlockFile: fileStream from: 150 to: 249.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/7/2006 22:47'!
testLockRegionForWrite05
	"Write lock, nonoverlapping regions, the local lock request should succeed"

	"(self selector: #testLockRegionForWrite05) run"

	| result |
	result _ self
		doRemote: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true]
		doLocal: [accessor lockFile: fileStream from: 200 to: 299 exclusive: true].
	self delay wait; wait.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (200 to: 299)).
	result _ accessor unlockFile: fileStream from: 200 to: 299.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (200 to: 299))! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:53'!
testLockRegionForWrite06
	"Write lock, overlapping regions, the local lock request should fail"

	"(self selector: #testLockRegionForWrite06) run"

	| result |
	result _ self
		doRemote: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true]
		doLocal: [accessor lockFile: fileStream from: 199 to: 199 exclusive: true].
	self delay wait; wait.
	self assert: result isNil.
	result _ accessor unlockFile: fileStream from: 199 to: 199.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:55'!
testLockRegionForWrite07
	"Write lock, overlapping regions, the lock check should fail"

	"(self selector: #testLockRegionForWrite07) run"

	| result |
	result _ self
		doRemote: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true]
		doLocal: [accessor isLockableFile: fileStream from: 100 to: 199 exclusive: true].
	self delay wait; wait.
	self deny: result.
	result _ accessor unlockFile: fileStream from: 100 to: 199.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/13/2006 09:55'!
testLockRegionForWrite08
	"Write lock, overlapping regions, the lock check should fail"

	"(self selector: #testLockRegionForWrite08) run"

	| result |
	result _ self
		doRemote: [accessor lockFile: fileStream from: 100 to: 199 exclusive: true]
		doLocal: [accessor isLockableFile: fileStream from: 150 to: 299 exclusive: true].
	self delay wait; wait.
	self deny: result.
	result _ accessor unlockFile: fileStream from: 150 to: 299.
	self assert: (self isValidUnlockResult: result)
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/7/2006 12:56'!
testWin32LockEmulation01
	"Make sure the cache does what is expected, and that it can be disabled
	Expect non-exclusive read locks to to succeed."

	"(self selector: #testWin32LockEmulation01) run"

	| result |
	accessor class emulateWin32FileLocking: true.
	result _ accessor lockFile: fileStream from: 100 to: 199 exclusive: false.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199)).
	result _ accessor lockFile: fileStream from: 100 to: 199 exclusive: false.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199)).
	result _ accessor unlockFile: fileStream from: 100 to: 199 exclusive: false.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199))
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 5/7/2006 12:56'!
testWin32LockEmulation02
	"Make sure the cache does what is expected, and that it can be disabled.
	Expect exclusive read-write locks to be protected by the cache."

	"(self selector: #testWin32LockEmulation02) run"

	| result |
	accessor class emulateWin32FileLocking: true.
	result _ accessor lockFile: fileStream from: 100 to: 199 exclusive: true.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199)).
	result _ accessor lockFile: fileStream from: 100 to: 199 exclusive: true.
	self assert: result isNil.
	result _ accessor unlockFile: fileStream from: 100 to: 199 exclusive: true.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199))
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 1/15/2018 21:55:55'!
testWin32LockEmulation03
	"Make sure the cache does what is expected, and that it can be disabled.
	Expect the cache to protect the file even if we open a second stream"

	"(self selector: #testWin32LockEmulation03) run"

	| result stream2 |
	accessor class emulateWin32FileLocking: true.
	result _ accessor lockFile: fileStream from: 100 to: 199 exclusive: true.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199)).
	stream2 _ OSProcess fileNamed: 'junkfile'.
	[result _ accessor lockFile: stream2 from: 100 to: 199 exclusive: true.
	self assert: result isNil]
		ensure: [stream2 close].
	result _ accessor unlockFile: fileStream from: 100 to: 199 exclusive: true.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199))
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'testing' stamp: 'dtl 1/15/2018 21:56:22'!
testWin32LockEmulation04
	"Make sure the cache does what is expected, and that it can be disabled.
	Expect the cache to allow a lock after the stream has been closed (since the OS will have freed its lock)"

	"(self selector: #testWin32LockEmulation04) run"

	| result stream2 |
	accessor class emulateWin32FileLocking: true.
	stream2 _ OSProcess fileNamed: 'junkfile'.
	[result _ accessor lockFile: stream2 from: 100 to: 199 exclusive: true.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199))]
		ensure: [stream2 close].
	result _ accessor lockFile: fileStream from: 100 to: 199 exclusive: true.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199)).
	result _ accessor unlockFile: fileStream from: 100 to: 199 exclusive: true.
	self assert: (result isKindOf: OSFileRegionLock).
	self assert: (result interval = (100 to: 199))! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'fail blocks' stamp: 'dtl 5/11/2006 07:22'!
testLockFileIfFail

	"(self selector: #testLockFileIfFail) run"

	| fail |
	fail _ false.
	accessor lockFile: fileStream ifFail: [fail _ true].
	self shouldnt: fail.
	fileStream close.
	accessor lockFile: fileStream ifFail: [fail _ true].
	self should: fail
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'fail blocks' stamp: 'dtl 11/28/2010 19:05'!
testLockFileRegionIfFail

	"(self selector: #testLockFileRegionIfFail) run"

	| fail |
	fail _ false.
	accessor
		lockFile: fileStream from: 100 to: 199 exclusive: true
		ifFail: [fail _ true].
	self shouldnt: fail.
	fileStream close.
	accessor
		lockFile: fileStream from: 100 to: 199 exclusive: true
		ifFail: [fail _ true].
	self should: fail.
	accessor
		lockFile: fileStream from: 100 to: 199
		ifFail: [fail _ true].
	self should: fail
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'fail blocks' stamp: 'dtl 5/11/2006 07:24'!
testUnlockFileIfFail

	"(self selector: #testUnlockFileIfFail) run"

	| fail |
	fail _ false.
	accessor lockFile: fileStream ifFail: [fail _ true].
	self shouldnt: fail.
	accessor unlockFile: fileStream ifFail: [fail _ true].
	self shouldnt: fail.
	fileStream close.
	accessor unlockFile: fileStream ifFail: [fail _ true].
	self should: fail
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'fail blocks' stamp: 'dtl 11/28/2010 19:11'!
testUnlockFileRegionIfFail

	"(self selector: #testUnlockFileRegionIfFail) run"

	| fail |
	fail _ false.
	accessor
		lockFile: fileStream from: 100 to: 199 exclusive: true
		ifFail: [fail _ true].
	self shouldnt: fail.
	accessor
		unlockFile: fileStream from: 100 to: 199 exclusive: true
		ifFail: [fail _ true].
	self shouldnt: fail.
	fileStream close.
	accessor
		unlockFile: fileStream from: 100 to: 199 exclusive: true
		ifFail: [fail _ true].
	self should: fail.
	accessor
		unlockFile: fileStream from: 100 to: 199
		ifFail: [fail _ true].
	self should: fail

! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'private' stamp: 'dtl 4/30/2006 13:47'!
tryUntil: aBlock
	"Repeat until block evaluates true, but do not get in an infinite loop if a primitive is failing"

	| result |
	10 timesRepeat:
		[result _ aBlock value.
		result ifTrue: [^ result].
		delay wait].
	^ result
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/13/2006 09:36'!
isValidUnlockResult: result

	self subclassResponsibility
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/10/2006 07:04'!
testLockPreviouslyLockedFile

	self subclassResponsibility! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/10/2006 07:10'!
testLockPreviouslyLockedFileRegion

	self subclassResponsibility! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/11/2006 06:53'!
testUnlockPreviouslyLockedFile

	self subclassResponsibility! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/11/2006 06:54'!
testUnlockPreviouslyLockedFileRegion

	self subclassResponsibility! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - exclusive locking' stamp: 'dtl 8/30/2009 15:55'!
testFailFileLockOnLockedFile
	"Two cooperating Squeak processes using mandatory file locking. When
	child Squeak takes a lock on a region, lock attempt in parent Squeak will
	fail."

	"(self selector: #testFailFileLockOnLockedFile) run"

	self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream) notNil]) ifNil: [self quitImage].
			delay wait; wait; wait.
			accessor unlockFile: fileStream.
			self quitImage]
		doLocal:
			[self should: [
				10 timesRepeat: [ "lock and unlock until failure due to child taking the lock"
					accessor lockFile: fileStream
							exclusive: true
							ifFail: [self error: '#lockFile failed because child Squeak now holds a lock'].
					accessor unlockFile: fileStream. "release lock to allow child to acquire it"
					delay wait]]
				raise: Error
				description: '#lockFile failed because child Squeak now holds a lock']
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - exclusive locking' stamp: 'dtl 8/30/2009 16:00'!
testFailLockOnLockedOverlappedRegion
	"Two cooperating Squeak processes using mandatory file locking. When
	child Squeak takes a lock on a region, lock attempt in parent Squeak will
	fail. Locked regions overlap by one character."

	"(self selector: #testFailLockOnLockedOverlappedRegion) run"

	self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream from: 100 to: 104) notNil]) ifNil: [self quitImage].
			delay wait; wait; wait.
			accessor unlockFile: fileStream from: 100 to: 104.
			self quitImage]
		doLocal:
			[self should: [
				10 timesRepeat: [ "lock and unlock until failure due to child taking the lock"
					accessor lockFile: fileStream
							from: 104
							to: 120
							exclusive: true
							ifFail: [self error: '#lockFile failed because child Squeak now holds a lock'].
					accessor unlockFile: fileStream from: 104 to: 120. "release lock to allow child to acquire it"
					delay wait]]
				raise: Error
				description: '#lockFile failed because child Squeak now holds a lock']
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - exclusive locking' stamp: 'dtl 8/30/2009 15:51'!
testFailLockOnLockedRegion
	"Two cooperating Squeak processes using mandatory file locking. When
	child Squeak takes a lock on a region, lock attempt in parent Squeak will
	fail."

	"(self selector: #testFailLockOnLockedRegion) run"

	self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream from: 100 to: 120) notNil]) ifNil: [self quitImage].
			delay wait; wait; wait.
			accessor unlockFile: fileStream from: 100 to: 120.
			self quitImage]
		doLocal:
			[self should: [
				10 timesRepeat: [ "lock and unlock until failure due to child taking the lock"
					accessor lockFile: fileStream
							from: 100
							to: 120
							exclusive: true
							ifFail: [self error: '#lockFile failed because child Squeak now holds a lock'].
					accessor unlockFile: fileStream from: 100 to: 120. "release lock to allow child to acquire it"
					delay wait]]
				raise: Error
				description: '#lockFile failed because child Squeak now holds a lock']
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - exclusive locking' stamp: 'dtl 8/30/2009 16:01'!
testFailLockOnLockedSupersetRegion
	"Two cooperating Squeak processes using mandatory file locking. When
	child Squeak takes a lock on a region, lock attempt in parent Squeak will
	fail. Fail when requesting a lock on a subset of the locked region."

	"(self selector: #testFailLockOnLockedSupersetRegion) run"

	self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream from: 100 to: 140) notNil]) ifNil: [self quitImage].
			delay wait; wait; wait.
			accessor unlockFile: fileStream from: 100 to: 140.
			self quitImage]
		doLocal:
			[self should: [
				10 timesRepeat: [ "lock and unlock until failure due to child taking the lock"
					accessor lockFile: fileStream
							from: 104
							to: 120
							exclusive: true
							ifFail: [self error: '#lockFile failed because child Squeak now holds a lock'].
					accessor unlockFile: fileStream from: 104 to: 120. "release lock to allow child to acquire it"
					delay wait]]
				raise: Error
				description: '#lockFile failed because child Squeak now holds a lock']
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - exclusive locking' stamp: 'dtl 8/30/2009 16:02'!
testFailRegionLockOnLockedFile
	"Two cooperating Squeak processes using mandatory file locking. When
	child Squeak takes a lock on a file, the region lock attempt in parent
	Squeak will fail."

	"(self selector: #testFailRegionLockOnLockedFile) run"

	self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream) notNil]) ifNil: [self quitImage].
			delay wait; wait; wait.
			accessor unlockFile: fileStream.
			self quitImage]
		doLocal:
			[self should: [
				10 timesRepeat: [ "lock and unlock until failure due to child taking the lock"
					accessor lockFile: fileStream
							from: 100
							to: 120
							exclusive: true
							ifFail: [self error: '#lockFile failed because child Squeak now holds a lock'].
					accessor unlockFile: fileStream from: 100 to: 120. "release lock to allow child to acquire it"
					delay wait]]
				raise: Error
				description: '#lockFile failed because child Squeak now holds a lock']
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - exclusive locking' stamp: 'dtl 8/30/2009 16:03'!
testNoFailLockOnAdjacentLockedRegions
	"Two cooperating Squeak processes using mandatory file locking. When
	child Squeak takes a lock on a region, lock attempt in parent Squeak will
	fail. No failure occurs when the regions are adjacent but do not overlap."

	"(self selector: #testNoFailLockOnAdjacentLockedRegions) run"

	self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream from: 100 to: 103) notNil]) ifNil: [self quitImage].
			delay wait; wait; wait.
			accessor unlockFile: fileStream from: 100 to: 103.
			self quitImage]
		doLocal:
			[self shouldnt: [
				10 timesRepeat: [ "lock and unlock until failure due to child taking the lock"
					accessor lockFile: fileStream
							from: 104
							to: 120
							exclusive: true
							ifFail: [self error: '#lockFile failed because child Squeak now holds a lock'].
					accessor unlockFile: fileStream from: 104 to: 120. "release lock to allow child to acquire it"
					delay wait]]
				raise: Error]
! !

!AbstractUnixProcessFileLockingTestCase methodsFor: 'tests - exclusive locking' stamp: 'dtl 8/30/2009 16:03'!
testNoFailLockOnDifferentLockedRegion
	"Two cooperating Squeak processes using mandatory file locking. When
	child Squeak takes a lock on a region, lock attempt in parent Squeak will
	fail. No failure occurs if the regions are different and do not overlap."

	"(self selector: #testNoFailLockOnDifferentLockedRegion) run"

	self
		doRemote:
			[(self tryUntil: [(accessor lockFile: fileStream from: 50 to: 80) notNil]) ifNil: [self quitImage].
			delay wait; wait; wait.
			accessor unlockFile: fileStream from: 50 to: 80.
			self quitImage]
		doLocal:
			[self shouldnt: [
				10 timesRepeat: [ "lock and unlock until failure due to child taking the lock"
					accessor lockFile: fileStream
							from: 104
							to: 120
							exclusive: true
							ifFail: [self error: '#lockFile failed because child Squeak now holds a lock'].
					accessor unlockFile: fileStream from: 104 to: 120. "release lock to allow child to acquire it"
					delay wait]]
				raise: Error]
! !

!AbstractUnixProcessFileLockingTestCase class methodsFor: 'testing' stamp: 'dtl 4/30/2006 13:49'!
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^ true! !

!UnixProcessUnixFileLockingTestCase methodsFor: 'running' stamp: 'dtl 4/30/2006 14:05'!
setUp

	"Set system to use Unix file locking semantics. The tearDown method is expected to restore the setting."

	OSProcessAccessor emulateWin32FileLocking: false.
	^ super setUp
! !

!UnixProcessUnixFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/13/2006 09:38'!
isValidUnlockResult: result
	"With no lock cache, the unlock requests will succeed even though there
	is no lock to be removed."	

	^ result isKindOf: OSFileLock
! !

!UnixProcessUnixFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/11/2006 07:12'!
testLockPreviouslyLockedFile
	"Locking a previously locked file is permitted"

	"(self selector: #testLockPreviouslyLockedFile) run"

	accessor lockFile: fileStream.
	self should: (accessor isLockableFile: fileStream).
	self should: ((accessor lockFile: fileStream) isKindOf: OSFileLock).
	accessor unlockFile: fileStream! !

!UnixProcessUnixFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/11/2006 07:12'!
testLockPreviouslyLockedFileRegion
	"Locking a previously locked file region is permitted"

	"(self selector: #testLockPreviouslyLockedFileRegion) run"

	accessor
		lockFile: fileStream
		from: 100
		to: 199
		exclusive: true.
	self should: (accessor
		isLockableFile: fileStream
		from: 100
		to: 199
		exclusive: true).
	self should: ((accessor
		lockFile: fileStream
		from: 100
		to: 199
		exclusive: true)
		isKindOf: OSFileLock).
	accessor unlockFile: fileStream! !

!UnixProcessUnixFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/11/2006 07:07'!
testUnlockPreviouslyLockedFile
	"The unlock operation will succeed even if no lock is in effect"

	"(self selector: #testUnlockPreviouslyLockedFile) run"

	self should: ((accessor unlockFile: fileStream) isKindOf: OSFileLock).
	accessor lockFile: fileStream.
	self should: ((accessor unlockFile: fileStream) isKindOf: OSFileLock)
! !

!UnixProcessUnixFileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/11/2006 07:07'!
testUnlockPreviouslyLockedFileRegion
	"The unlock operation will succeed even if no lock is in effect"

	"(self selector: #testUnlockPreviouslyLockedFileRegion) run"

	self should: ((accessor
		unlockFile: fileStream
		from: 100
		to: 199
		exclusive: true) isKindOf: OSFileLock).
	accessor
		lockFile: fileStream
		from: 100
		to: 199
		exclusive: true.
	self should: ((accessor
		unlockFile: fileStream
		from: 100
		to: 199
		exclusive: true) isKindOf: OSFileLock)
! !

!UnixProcessUnixFileLockingTestCase class methodsFor: 'testing' stamp: 'dtl 1/13/2007 15:40'!
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self name = #TestCase
! !

!UnixProcessWin32FileLockingTestCase methodsFor: 'running' stamp: 'dtl 4/30/2006 14:06'!
setUp

	"Set system to use Windows file locking semantics. The tearDown method is expected to restore the setting."

	OSProcessAccessor emulateWin32FileLocking: true.
	^ super setUp
! !

!UnixProcessWin32FileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/13/2006 09:47'!
isValidUnlockResult: result
	"With a lock cache, the unlock requests will fail when if there
	is no lock to be removed."	

	^ result == nil
! !

!UnixProcessWin32FileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/11/2006 07:13'!
testLockPreviouslyLockedFile
	"A previously locked file cannot be locked."

	"(self selector: #testLockPreviouslyLockedFile) run"

	accessor lockFile: fileStream.
	self shouldnt: (accessor isLockableFile: fileStream).
	self should: (accessor lockFile: fileStream) isNil.
	accessor unlockFile: fileStream! !

!UnixProcessWin32FileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/11/2006 07:13'!
testLockPreviouslyLockedFileRegion
	"A previously locked file region cannot be locked."

	"(self selector: #testLockPreviouslyLockedFileRegion) run"

	accessor
		lockFile: fileStream
		from: 100
		to: 199
		exclusive: true.
	self shouldnt: (accessor
		isLockableFile: fileStream
		from: 100
		to: 199
		exclusive: true).
	self should: ((accessor
		lockFile: fileStream
		from: 100
		to: 199
		exclusive: true) isNil).
	accessor unlockFile: fileStream! !

!UnixProcessWin32FileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/11/2006 07:08'!
testUnlockPreviouslyLockedFile
	"The unlock operation will fail if lock is in effect"

	"(self selector: #testUnlockPreviouslyLockedFile) run"

	self shouldnt: ((accessor unlockFile: fileStream) isKindOf: OSFileLock).
	accessor lockFile: fileStream.
	self should: ((accessor unlockFile: fileStream) isKindOf: OSFileLock).
	self shouldnt: ((accessor unlockFile: fileStream) isKindOf: OSFileLock)
! !

!UnixProcessWin32FileLockingTestCase methodsFor: 'tests - platform specific' stamp: 'dtl 5/12/2006 07:48'!
testUnlockPreviouslyLockedFileRegion
	"The unlock operation will fail if a lock is in effect"

	"(self selector: #testUnlockPreviouslyLockedFileRegion) run"

	self shouldnt: ((accessor
		unlockFile: fileStream
		from: 100
		to: 199
		exclusive: true) isKindOf: OSFileLock).
	accessor
		lockFile: fileStream
		from: 100
		to: 199
		exclusive: true.
	self should: ((accessor
		unlockFile: fileStream
		from: 100
		to: 199
		exclusive: true) isKindOf: OSFileLock).
	self shouldnt: ((accessor
		unlockFile: fileStream
		from: 100
		to: 199
		exclusive: true) isKindOf: OSFileLock)
! !

!UnixProcessWin32FileLockingTestCase class methodsFor: 'testing' stamp: 'dtl 1/13/2007 15:40'!
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self name = #TestCase
! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 8/19/2022 23:26'!
testEnableHandleAndDisable

	| eventHandler anOpenFile fileHandle aioHandleResult sema semaIndex aioEnableResult aioDisableResult |
	eventHandler _ AioEventHandler new.
	anOpenFile _ OSProcess fileNamed: 'delete.me'.
	fileHandle _ eventHandler descriptorForFile: anOpenFile.
	sema _ Semaphore new.
	[semaIndex _ Smalltalk registerExternalObject: sema.
	[aioEnableResult _ eventHandler
			aioEnable: fileHandle
			forSemaphore: semaIndex
			externalObject: true]
		on: Warning
		do: ["Suppress warning dialog. Unit test will fail next assert."].
	self assert: semaIndex == aioEnableResult.
	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: true
		readEvents: true
		writeEvents: true.
	self assert: aioHandleResult == 2r0111. "return bitmask, all three bits set"
	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: true
		readEvents: false
		writeEvents: true.
	self assert: aioHandleResult == 2r0101.
	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: false
		writeEvents: true.
	self assert: aioHandleResult == 2r0100.
	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: true
		readEvents: false
		writeEvents: false.
	self assert: aioHandleResult == 2r0001.
	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: false
		writeEvents: false.
	self assert: aioHandleResult == 2r0000.
	aioDisableResult _ eventHandler aioDisable: fileHandle.
	self assert: aioDisableResult == fileHandle]
		ensure:
			[Smalltalk unregisterExternalObject: sema.
			(fileHandle isKindOf: Integer) ifTrue: [eventHandler aioDisable: fileHandle]].
	anOpenFile close.
	OSProcess deleteFileNamed: 'delete.me'.
! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 8/19/2022 23:26'!
testFileReadableEvent

	| eventHandler anOpenFile fileHandle aioHandleResult sema semaIndex aioEnableResult fileName |
	fileName _ 'DeleteThisTemporaryFile'.

	eventHandler _ AioEventHandler new.
	OSProcess deleteFileNamed: fileName.
	[anOpenFile _ OSProcess fileNamed: fileName.
	fileHandle _ eventHandler descriptorForFile: anOpenFile.
	sema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: sema.
	[aioEnableResult _ eventHandler
			aioEnable: fileHandle
			forSemaphore: semaIndex
			externalObject: true]
		on: Warning
		do: ["Suppress warning dialog. Unit test will fail next assert."].
	self assert: semaIndex == aioEnableResult.

	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.
	self assert: aioHandleResult == 2r010.
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self assert: sema isSignaled. "File is empty but readable. A signal is sent once,
	presumably in the interest of priming the pump. A listening process would
	begin reading data based on having received this initial signal."
	sema wait. "consume the signal"
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self deny: sema isSignaled. "Nothing has happened to indicate more data available."

	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.
	self assert: aioHandleResult == 2r010.
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self assert: sema isSignaled. "File is empty but readable. A signal is sent once,
	presumably in the interest of priming the pump. A listening process would
	begin reading data based on having received this initial signal."
	sema wait. "consume the signal"
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self deny: sema isSignaled. "Nothing has happened to indicate more data available."

	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.
	self assert: aioHandleResult == 2r010.
	anOpenFile nextPutAll: 'write some stuff to the file'; flush.
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self assert: sema isSignaled. "File is empty but readable. A signal is sent once,
	presumably in the interest of priming the pump. A listening process would
	begin reading data based on having received this initial signal."
	sema wait. "consume the signal"

	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.
	self assert: aioHandleResult == 2r010.
	anOpenFile upToEnd.
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self assert: sema isSignaled. "File is empty but readable. A signal is sent once,
	presumably in the interest of priming the pump. A listening process would
	begin reading data based on having received this initial signal."
	sema wait. "consume the signal"

	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self deny: sema isSignaled. "handler has not been reset"]
		ensure:
			[[anOpenFile close] on: Error do: [].
			Smalltalk unregisterExternalObject: sema.
			(fileHandle isKindOf: Integer) ifTrue: [eventHandler aioDisable: fileHandle]]! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 8/19/2022 23:26'!
testFileWritableEvent

	| eventHandler anOpenFile fileHandle aioHandleResult sema semaIndex aioEnableResult fileName |
	fileName _ 'DeleteThisTemporaryFile'.

	eventHandler _ AioEventHandler new.
	OSProcess deleteFileNamed: fileName.
	[anOpenFile _ OSProcess fileNamed: fileName.
	fileHandle _ eventHandler descriptorForFile: anOpenFile.
	sema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: sema.
	[aioEnableResult _ eventHandler
			aioEnable: fileHandle
			forSemaphore: semaIndex
			externalObject: true]
		on: Warning
		do: ["Suppress warning dialog. Unit test will fail next assert."].
	self assert: semaIndex == aioEnableResult.

	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: false
		writeEvents: true.
	self assert: aioHandleResult == 2r0100.
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self assert: sema isSignaled. "File is empty but writable. A signal is sent once,
	presumably in the interest of priming the pump. A listening process would
	begin writing data based on having received this initial signal."
	sema wait. "consume the signal"
	self deny: sema isSignaled.

	"Reset handler"
	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: false
		writeEvents: true.
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self assert: sema isSignaled. "File is empty but writable. A signal is sent once,
	presumably in the interest of priming the pump. A listening process would
	begin writing data based on having received this initial signal."
	sema wait. "consume the signal"
	self deny: sema isSignaled.

	"Reset handler"
	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: false
		writeEvents: true.
	anOpenFile upToEnd.
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self assert: sema isSignaled. "File is empty but writable. A signal is sent once,
	presumably in the interest of priming the pump. A listening process would
	begin writing data based on having received this initial signal."
	sema wait. "consume the signal"
	self deny: sema isSignaled.

	"Reset handler"
	aioHandleResult _ eventHandler
		aioHandle: fileHandle
		exceptionEvents: false
		readEvents: false
		writeEvents: true.
	anOpenFile nextPutAll: 'write some stuff to the file'; flush.
	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self assert: sema isSignaled. "File is empty but writable. A signal is sent once,
	presumably in the interest of priming the pump. A listening process would
	begin writing data based on having received this initial signal."
	sema wait. "consume the signal"
	self deny: sema isSignaled.

	(Delay forMilliseconds: 20) wait. "Allow signals to be handled"
	self deny: sema isSignaled. "handler has not been reset"]
		ensure:
			[[anOpenFile close] on: Error do: [].
			Smalltalk unregisterExternalObject: sema.
			(fileHandle isKindOf: Integer) ifTrue: [eventHandler aioDisable: fileHandle]]! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 8/19/2022 23:26'!
testHandleForFile

	| eventHandler anOpenFile fileHandle |
	eventHandler _ AioEventHandler new.
	anOpenFile _ OSProcess fileNamed: 'delete.me'.
	fileHandle _ eventHandler descriptorForFile: anOpenFile.
	anOpenFile close.
	OSProcess deleteFileNamed: 'delete.me'.
	self assert: fileHandle notNil.
	self assert: (fileHandle isKindOf: Integer)
! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 8/19/2022 23:28'!
testHandleForSocket

	| eventHandler socket socketHandle |
	eventHandler _ AioEventHandler new.
	socket _ Socket newTCP.
	socketHandle _ eventHandler desriptorForSocket: socket.
	self assert: socketHandle notNil.
	self assert: (socketHandle isKindOf: Integer).
	self assert: (socketHandle ~= 0) "0 is stdin on unix, unlikely to be used on other platforms"
! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 7/9/2005 16:41'!
testPrimAioModuleName

	| eventHandler moduleName |
	eventHandler _ AioEventHandler new.
	moduleName _ eventHandler primAioModuleName.
	self assert: ('AioPlugin*' match: moduleName)
! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 7/9/2005 16:42'!
testPrimAioModuleVersionString

	| eventHandler versionString |
	eventHandler _ AioEventHandler new.
	versionString _ eventHandler primAioModuleName.
	self assert: (versionString isKindOf: String)
! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 12/18/2022 20:15'!
testSocketExceptionEvent
	"Close the client socket to generate an event on the server socket"

	| port serverName serverTcpSocket clientTcpSocket sema semaIndex socketHandle eventHandler receivedData |
	port _ 8086.
	serverName _ '127.0.0.1'.
	Socket initializeNetwork.

	"Create the server (reader) socket"
	serverTcpSocket _ Socket newTCP.
	serverTcpSocket listenOn: port.

	"Create the client (writer) socket"
	clientTcpSocket _ Socket newTCP.
	clientTcpSocket connectTo: (NetNameResolver addressFromString: serverName) port: port.

	"Esstablish socket connection"
	[serverTcpSocket waitForConnectionFor: 10] fork.
	clientTcpSocket waitForConnectionFor: 10.

	["Set up a read event handler on the server socket"
	sema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: sema.
	eventHandler _ AioEventHandler new.
	socketHandle _ eventHandler desriptorForSocket: serverTcpSocket.

	"An aio handler may already have been established for the socket
	by the SocketPlugin. The following request disables any existing handling
	in order to prevent a warning message on the console stderr."
	eventHandler aioDisable: socketHandle.

	"Now set our own handler."
	[eventHandler
			aioEnable: socketHandle
			forSemaphore: semaIndex
			externalObject: true]
		on: Warning
		do: ["Suppress warning dialog. Unit test will fail next assert."].
	eventHandler
		aioHandle: socketHandle
		exceptionEvents: true
		readEvents: false
		writeEvents: false.
	self deny: sema isSignaled.
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	self deny: sema isSignaled.
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.
	self deny: sema isSignaled.
	receivedData _ serverTcpSocket receiveAvailableData.
	clientTcpSocket close.
	(Delay forMilliseconds: 200) wait.
	receivedData _ serverTcpSocket receiveAvailableData.
	(Delay forMilliseconds: 200) wait.
"	self assert: sema isSignaled.	FIXME: no error generated; need to find a way to generate an error -dtl"
	self deny: sema isSignaled.
	serverTcpSocket close]
		ensure:
			[Smalltalk unregisterExternalObject: sema.
			(socketHandle isKindOf: Integer) ifTrue: [eventHandler aioDisable: socketHandle]]! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 12/18/2022 20:15'!
testSocketExceptionEvent2
	"Close the client socket to generate an event on the server socket"

	| port serverTcpSocket sema semaIndex socketHandle eventHandler |
	port _ 8086.
	Socket initializeNetwork.

	"Create the server (reader) socket"
	serverTcpSocket _ Socket newTCP.
	serverTcpSocket listenOn: port.

	["Set up an exception event handler on the server socket"
	sema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: sema.
	eventHandler _ AioEventHandler new.
	socketHandle _ eventHandler desriptorForSocket: serverTcpSocket.

	"An aio handler may already have been established for the socket
	by the SocketPlugin. The following request disables any existing handling
	in order to prevent a warning message on the console stderr."
	eventHandler aioDisable: socketHandle.

	"Now set our own handler."
	[eventHandler
			aioEnable: socketHandle
			forSemaphore: semaIndex
			externalObject: true]
		on: Warning
		do: ["Suppress warning dialog. Unit test will fail next assert."].
	eventHandler
		aioHandle: socketHandle
		exceptionEvents: true
		readEvents: false
		writeEvents: true.
	self deny: sema isSignaled.
	[serverTcpSocket
		primSocket: serverTcpSocket socketHandle
		sendData: 'a string to send' asByteArray
		startIndex: 1
		count: 10] on: Error do: [self halt].
	serverTcpSocket
		receiveSomeDataInto: (String new: 1000)
		startingAt: 1.

"	self assert: sema isSignaled.	FIXME: no error generated; need to find a way to generate an error -dtl"
	self deny: sema isSignaled.

	serverTcpSocket close]
		ensure:
			[Smalltalk unregisterExternalObject: sema.
			(socketHandle isKindOf: Integer) ifTrue: [eventHandler aioDisable: socketHandle]]! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 12/18/2022 20:16'!
testSocketReadableEvent

	| port serverName serverTcpSocket clientTcpSocket sema semaIndex socketHandle eventHandler receivedData |
	port _ 8086.
	serverName _ '127.0.0.1'.
	Socket initializeNetwork.

	"Create the server (reader) socket"
	serverTcpSocket _ Socket newTCP.
	serverTcpSocket listenOn: port.

	"Create the client (writer) socket"
	clientTcpSocket _ Socket newTCP.
	clientTcpSocket connectTo: (NetNameResolver addressFromString: serverName) port: port.

	"Establish socket connection"
	[serverTcpSocket waitForConnectionFor: 10] fork.
	clientTcpSocket waitForConnectionFor: 10.

	["Set up a read event handler on the server socket"
	sema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: sema.
	eventHandler _ AioEventHandler new.
	socketHandle _ eventHandler desriptorForSocket: serverTcpSocket.

	"An aio handler may already have been established for the socket
	by the SocketPlugin. The following request disables any existing handling
	in order to prevent a warning message on the console stderr."
	eventHandler aioDisable: socketHandle.

	"Now set our own handler."
	[eventHandler
			aioEnable: socketHandle
			forSemaphore: semaIndex
			externalObject: true]
		on: Warning
		do: ["Suppress warning dialog. Unit test will fail next assert."].

	eventHandler
		aioHandle: socketHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.
	self deny: sema isSignaled.
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	self assert: sema isSignaled.
	sema wait. "consume the signal"
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.

	eventHandler
		aioHandle: socketHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.
	self deny: sema isSignaled.
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	self assert: sema isSignaled.
	sema wait. "consume the signal"
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.

	eventHandler
		aioHandle: socketHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.
	self deny: sema isSignaled.
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	self assert: sema isSignaled.
	sema wait. "consume the signal"
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.

	clientTcpSocket close.
	serverTcpSocket close]
		ensure:
			[Smalltalk unregisterExternalObject: sema.
			(socketHandle isKindOf: Integer) ifTrue: [eventHandler aioDisable: socketHandle]]! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 12/18/2022 20:16'!
testSocketReadableEventWithMaskNotSet
	"Same as testSocketReadableEvent, except that the mask bit for read events is not set,
	so we expect to not receive the event notification. This is just a check to ensure that
	the event mask is working."

	| port serverName serverTcpSocket clientTcpSocket sema semaIndex socketHandle eventHandler receivedData |
	port _ 8086.
	serverName _ '127.0.0.1'.
	Socket initializeNetwork.

	"Create the server (reader) socket"
	serverTcpSocket _ Socket newTCP.
	serverTcpSocket listenOn: port.

	"Create the client (writer) socket"
	clientTcpSocket _ Socket newTCP.
	clientTcpSocket connectTo: (NetNameResolver addressFromString: serverName) port: port.

	"Establish socket connection"
	[serverTcpSocket waitForConnectionFor: 10] fork.
	clientTcpSocket waitForConnectionFor: 10.

	["Set up a read event handler on the server socket"
	sema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: sema.
	eventHandler _ AioEventHandler new.
	socketHandle _ eventHandler desriptorForSocket: serverTcpSocket.

	"An aio handler may already have been established for the socket
	by the SocketPlugin. The following request disables any existing handling
	in order to prevent a warning message on the console stderr."
	eventHandler aioDisable: socketHandle.

	"Now set our own handler."
	[eventHandler
			aioEnable: socketHandle
			forSemaphore: semaIndex
			externalObject: true]
		on: Warning
		do: ["Suppress warning dialog. Unit test will fail next assert."].
	eventHandler
		aioHandle: socketHandle
		exceptionEvents: true
		readEvents: false	"Read event bit not set"
		writeEvents: false.
	self deny: sema isSignaled.
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	self deny: sema isSignaled. "This time we should not receive a signal"
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.
	clientTcpSocket close.
	serverTcpSocket close]
		ensure:
			[Smalltalk unregisterExternalObject: sema.
			(socketHandle isKindOf: Integer) ifTrue: [eventHandler aioDisable: socketHandle]]! !

!AioEventHandlerTestCase methodsFor: 'tests' stamp: 'dtl 12/18/2022 20:17'!
testSuspendAioForSocketReadableEvent

	| port serverName serverTcpSocket clientTcpSocket sema semaIndex socketHandle eventHandler receivedData |
	port _ 8086.
	serverName _ '127.0.0.1'.
	Socket initializeNetwork.

	"Create the server (reader) socket"
	serverTcpSocket _ Socket newTCP.
	serverTcpSocket listenOn: port.

	"Create the client (writer) socket"
	clientTcpSocket _ Socket newTCP.
	clientTcpSocket connectTo: (NetNameResolver addressFromString: serverName) port: port.

	"Establish socket connection"
	[serverTcpSocket waitForConnectionFor: 10] fork.
	clientTcpSocket waitForConnectionFor: 10.

	["Set up a read event handler on the server socket"
	sema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: sema.
	eventHandler _ AioEventHandler new.
	socketHandle _ eventHandler desriptorForSocket: serverTcpSocket.

	"An aio handler may already have been established for the socket
	by the SocketPlugin. The following request disables any existing handling
	in order to prevent a warning message on the console stderr."
	eventHandler aioDisable: socketHandle.

	"Now set our own handler."
	[eventHandler
			aioEnable: socketHandle
			forSemaphore: semaIndex
			externalObject: true]
		on: Warning
		do: ["Suppress warning dialog. Unit test will fail next assert."].

	eventHandler
		aioHandle: socketHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.

	self deny: sema isSignaled.

	"Handler generates a signal"
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.
	self assert: sema isSignaled.
	sema wait.
	self deny: sema isSignaled.

	"Handler has not been reset, so no signal this time"
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.
	self deny: sema isSignaled.

	"Reset the handler"
	eventHandler
		aioHandle: socketHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.

	"Handler generates a signal"
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.
	self assert: sema isSignaled.
	sema wait.
	self deny: sema isSignaled.

	"Reset the handler"
	eventHandler
		aioHandle: socketHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.
	"But temporarily disable it"
	eventHandler aioSuspend: socketHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.

	"Handler suspended, so no signal this time"
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.
	self deny: sema isSignaled.

	"Reset the handler"
	eventHandler
		aioHandle: socketHandle
		exceptionEvents: false
		readEvents: true
		writeEvents: false.

	"Handler generates a signal"
	clientTcpSocket sendData: 'some data to send to the socket' asByteArray.
	(Delay forMilliseconds: 200) wait.
	receivedData _ serverTcpSocket receiveAvailableData.
	self assert: 'some data to send to the socket' = receivedData.
	self assert: sema isSignaled.
	sema wait.
	self deny: sema isSignaled.

	clientTcpSocket close.
	serverTcpSocket close]
		ensure:
			[Smalltalk unregisterExternalObject: sema.
			(socketHandle isKindOf: Integer) ifTrue: [eventHandler aioDisable: socketHandle]]! !

!OSPipeTestCase methodsFor: 'private' stamp: 'dtl 11/28/2010 14:05'!
primTestEndOfFileFlag: aSqFileStruct
	"Answer whether the file represented by aSqFileStruct is at end of file, as determined
	by a call to feof(). This is different from StandardFileStream>>primAtEnd: which answers
	true if the file pointer is at the end of the file, but which does not call feof() to
	determine that an end of file condition has occurred. The difference is significant
	if aSqFileStruct represents a pipe or a device file, which may not be positionable
	in the sense of a conventional disk file."

	<primitive: 'primitiveTestEndOfFileFlag' module: 'UnixOSProcessPlugin'>
	^ nil! !

!OSPipeTestCase methodsFor: 'private' stamp: 'dtl 6/4/2006 16:03'!
readFromAndClose: aPipe writingTo: aStream

	| s |
	[aPipe atEndOfFile] whileFalse:
		[s _ aPipe next: 10000.
		aStream nextPutAll: s asString.
		(Delay forMilliseconds: 100) wait].
	(aPipe respondsTo: #reader) ifTrue: [aPipe reader close].
	^ aStream
! !

!OSPipeTestCase methodsFor: 'private' stamp: 'dtl 3/25/2006 18:29'!
writeStuffOnThenClose: aPipe

	^ [(1 to: 10) do:
		[:i |
		[aPipe nextPutAll: 'this is line ', i printString; cr]
			on: Error
			do: ["Ignore error. Test case will still fail, and throwing an error in this
				process would cause a debugger to be scheduled inconveniently."].
		(Delay forMilliseconds: 50) wait].
	(aPipe respondsTo: #writer)
		ifTrue: [[aPipe writer close]
					on: Error
					do: ["Ignore error to avoid a debugger"]]
		ifFalse: [[aPipe close]
					on: Error
					do: ["Ignore error to avoid a debugger"]]]
		forkAt: Processor userBackgroundPriority! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 4/12/2014 16:07'!
testBlocking

	"(OSPipeTestCase selector: #testBlocking) run"

	| p |
	p _ OSPipe blockingPipe.
	(1 to: 10) do: [:i | p nextPutAll: 'this is line ', i printString; cr].
	p flush. "protect against buggy clib that may never flush output"
	self should: ['this is line 1*' match: (p next: 15)].
	self should: ['this is line 2*' match: (p next: 15)].
	self should: ['this is line 3*' match: (p next: 15)].
	self should: ['this is line 4*' match: (p next: 15)].
	self should: ['this is line 5*' match: (p next: 15)].
	self should: ['this is line 6*' match: (p next: 15)].
	self should: ['this is line 7*' match: (p next: 15)].
	self should: ['this is line 8*' match: (p next: 15)].
	self should: ['this is line 9*' match: (p next: 15)].
	p writer close.
	self shouldnt: p atEnd.
	self should: ['this is line 10*' match: (p next: 16)].
	self should: p atEnd.
	p close
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 6/6/2015 09:27'!
testBlockingBufferedPipe

	"(OSPipeTestCase selector: #testBlockingBufferedPipe) run"

	| p |
	p _ ExternalPipe bufferedBlockingPipe.
	[(1 to: 10) do: [:i | p nextPutAll: 'this is line ', i printString; cr; flush].
	(Delay forMilliseconds: 100) wait. "allow pipe reads to complete"
	self should: ['this is line 1*' match: (p next: 15)].
	self should: ['this is line 2*' match: (p next: 15)].
	self should: ['this is line 3*' match: (p next: 15)].
	self should: ['this is line 4*' match: (p next: 15)].
	self should: ['this is line 5*' match: (p next: 15)].
	self should: ['this is line 6*' match: (p next: 15)].
	self should: ['this is line 7*' match: (p next: 15)].
	self should: ['this is line 8*' match: (p next: 15)].
	self should: ['this is line 9*' match: (p next: 15)].
	p writer close.
	self shouldnt: p atEnd.
	self should: ['this is line 10*' match: (p next: 16)].
	self should: p atEnd]
		ensure: [p close]
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 12/16/2022 19:55'!
testBlockingPeek

	"(OSPipeTestCase selector: #testBlockingPeek) run"

	| p |
	p _ OSPipe blockingPipe.
	p nextPutAll: 'abc'.
	p flush. "protect against buggy clib that may never flush output"
	self assert: (p peek = $a).
	self assert: (p next = $a).
	self assert: (p peek = $b).
	self assert: (p next = $b).
	self assert: (p peek = $c).
	self deny: p atEnd.
	self assert: (p next = $c).
	self deny: p atEnd.
	p closeWriter.
	self assert: p atEnd.
	p close
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 12/16/2022 19:56'!
testBlockingPeekBufferedPipe

	"(OSPipeTestCase selector: #testBlockingPeekBufferedPipe) run"

	| p |
	p _ ExternalPipe bufferedBlockingPipe.
	[p nextPutAll: 'abc'; flush.
	(Delay forMilliseconds: 100) wait. "allow pipe reads to complete"
	self assert: (p peek = $a).
	self assert: (p next = $a).
	self assert: (p peek = $b).
	self assert: (p next = $b).
	self assert: (p peek = $c).
	self deny: p atEnd.
	self assert: (p next = $c).
	self deny: p atEnd.
	p closeWriter.
	self assert: p atEnd]
		ensure: [p close]
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 5/17/2006 23:36'!
testBufferedUpToEnd

	"(OSPipeTestCase selector: #testBufferedUpToEnd) debug"

	| s p r1 r2 |
	s _ 'some data to put through the pipe'.
	p _ OSPipe new.
	self primTestEndOfFileFlag: p reader fileID. "check that OSPP is updated"
	self assert: p setBufferedReader.
	[p nextPutAll: s.
	p closeWriter.
	r1 _ p upToEnd. "#atEnd does not detect EOF on a pipe"
	self assert: r1 isEmpty.
	r2 _ p upToEndOfFile. "detects EOF correctly, gets remaining data"
	self assert: r2 = s]
		ensure: [p close]
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 12/16/2022 19:57'!
testIsAtEndOfFile

	"(OSPipeTestCase selector: #testIsAtEndOfFile) debug"

	| p |
	p _ OSPipe new.
	self primTestEndOfFileFlag: p reader fileID. "check that OSPP is updated"
	self deny: (OSProcess accessor isAtEndOfFile: p reader fileID).
	self deny: p reader atEnd.
	self deny: p atEnd.
	self assert: (p reader basicNext == nil).
	self deny: (OSProcess accessor isAtEndOfFile: p reader fileID).
	self deny: p reader atEnd.
	self deny: p atEnd.
	p writer nextPut: $X; flush.
	self assert: (p reader basicNext = $X).
	self deny: (OSProcess accessor isAtEndOfFile: p reader fileID).
	self deny: p reader atEnd.
	self deny: p atEnd.
	self assert: (p reader basicNext == nil).
	self deny: (OSProcess accessor isAtEndOfFile: p reader fileID).
	self deny: p reader atEnd.
	self deny: p atEnd.

	p writer close.
	self deny: (OSProcess accessor isAtEndOfFile: p reader fileID).
	self deny: p atEndOfFile. "no read yet, so flag has not been set"
	self deny: p reader atEnd description: 'Failure here is likely caused by stdio hacks in sqFileAtEnd() support code. Don''t do that.'.
	self assert: p atEnd. "writer closed and read gives nil"

	self assert: (p reader basicNext == nil). "read operation should set flag"
	self assert: (OSProcess accessor isAtEndOfFile: p reader fileID).
	self deny: p reader atEnd. "#atEnd does not detect EOF on a pipe"
	self assert: p reader atEndOfFile.
	self assert: p atEnd.
	p close
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 6/6/2015 09:23'!
testIsAtEndOfFile2

	"(OSPipeTestCase selector: #testIsAtEndOfFile2) debug"

	| p s |
	p _ OSPipe new.
	[p nextPutAll: 'hello'; flush.
	s _ p next: 100.
	self assert: s = 'hello'.
	self deny: p atEndOfFile.
	p closeWriter.
	self deny: p atEndOfFile.
	s _ p next: 100.
	self assert: s = ''.
	self assert: p atEndOfFile]
		ensure: [p close]
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 3/25/2006 13:44'!
testNonBlocking

	"(OSPipeTestCase selector: #testNonBlocking) run"

	| p ws str |
	p _ OSPipe nonBlockingPipe.
	self writeStuffOnThenClose: p.
	ws _ self readFromAndClose: p writingTo: (WriteStream on: String new).
	str _ (ws contents last: 16).
	self should: ['this is line 10*' match: str].
	p close
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 4/2/2006 21:40'!
testNonBlockingBufferedPipe

	"(OSPipeTestCase selector: #testNonBlockingBufferedPipe) run"

	| p ws str |
	p _ ExternalPipe bufferedNonBlockingPipe.
	[self writeStuffOnThenClose: p.
	self assert: (p peek == nil).
	(Delay forMilliseconds: 100) wait. "allow pipe reads to complete"
	ws _ self readFromAndClose: p writingTo: (WriteStream on: String new).
	str _ (ws contents last: 16).
	self should: ['this is line 10*' match: str]]
		ensure: [p close]
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 12/16/2022 19:57'!
testNonBlockingPeek

	"(OSPipeTestCase selector: #testNonBlockingPeek) run"

	| p |
	p _ OSPipe nonBlockingPipe.
	p nextPutAll: 'abc'; flush.
	self assert: (p peek = $a).
	self assert: (p next = $a).
	self assert: (p peek = $b).
	self assert: (p next = $b).
	self assert: (p peek = $c).
	self deny: p atEnd.
	self assert: (p next = $c).
	self deny: p atEnd.
	p closeWriter.
	self assert: p atEnd.
	p close
! !

!OSPipeTestCase methodsFor: 'tests' stamp: 'dtl 12/16/2022 19:58'!
testNonBlockingPeekBufferedPipe

	"(OSPipeTestCase selector: #testNonBlockingPeekBufferedPipe) run"

	| p |
	p _ ExternalPipe bufferedNonBlockingPipe.
	[p nextPutAll: 'abc'; flush.
	self assert: (p peek = nil).
	(Delay forMilliseconds: 100) wait. "allow pipe reads to complete"
	self assert: (p peek = $a).
	self assert: (p next = $a).
	self assert: (p peek = $b).
	self assert: (p next = $b).
	self assert: (p peek = $c).
	self deny: p atEnd.
	self assert: (p next = $c).
	self deny: p atEnd.
	p closeWriter.
	self assert: p atEnd]
		ensure: [p close]
! !

!ResettableBufferStreamTest methodsFor: 'tests' stamp: 'dtl 7/23/2023 11:15'!
testReadWrite
	"Basic read and write behavior"

	"(ResettableBufferStreamTest selector: #testReadWrite) debug"

	| strm |
	strm _ ResettableBufferStream with: 'some characters'.
	self assert: 15 equals: strm position.
	self assert: 15 equals: strm size.
	self assert: 'some characters' equals: strm contents.
	strm nextPutAll: ' XXX'.
	self assert: 19 equals: strm position.
	self assert: 19 equals: strm size.
	self assert: 'some characters XXX' equals: strm contents.
	self assert: '' equals: strm upToEnd.
	strm position: 10.
	self assert: 'cters XXX' equals: strm upToEnd.


! !

!ResettableBufferStreamTest methodsFor: 'tests' stamp: 'dtl 7/23/2023 11:51'!
testRemovePreviouslyRead
	"Basic read and write behavior"

	"(ResettableBufferStreamTest selector: #testRemovePreviouslyRead) debug"

	| strm |
	strm _ ResettableBufferStream with: 'some characters'.
	self assert: 15 equals: strm position.
	self assert: 15 equals: strm size.
	self assert: 'some characters' equals: strm contents.
	strm reset.
	strm removePreviouslyRead.
	self assert: 0 equals: strm position.
	self assert: 15 equals: strm size.
	self assert: 'some characters' equals: strm contents.
	strm reset; next: 5.
	strm removePreviouslyRead.
	self assert: 0 equals: strm position.
	self assert: 10 equals: strm size.
	self assert: 'characters' equals: strm contents.
	self assert: 'characters' equals: strm upToEnd.
	self assert: 10 equals: strm position.
	strm removePreviouslyRead.
	self assert: 0 equals: strm position.
	self assert: 0 equals: strm size.
	self assert: '' equals: strm contents.
	self assert: '' equals: strm upToEnd.
	self assert: 0 equals: strm position.
	strm nextPutAll: 'XXX'.
	strm reset.
	self assert: 'XXX' equals: strm contents.
	self assert: 'XXX' equals: strm upToEnd.

! !

!UnixProcessAccessorTestCase methodsFor: 'running' stamp: 'dtl 1/1/2024 10:33'!
contentsOfFile: path
	| fs contents |
	fs _ OSProcess oldFileNamed: path.
	contents _ fs contentsOfEntireFile.
	fs close.
	^ contents
! !

!UnixProcessAccessorTestCase methodsFor: 'running' stamp: 'dtl 3/1/2002 18:02'!
runAll

	"UnixProcessAccessorTestCase new runAll"

	| result suite |
	suite _ TestSuite new.
	suite addTest: (UnixProcessAccessorTestCase selector: #testSessionIdentifier).
	suite addTest: (UnixProcessAccessorTestCase selector: #testCanAccessSystem).
	suite addTest: (UnixProcessAccessorTestCase selector: #testMakePipeHandles).
	suite addTest: (UnixProcessAccessorTestCase selector: #testUnixFileNumber).
	suite addTest: (UnixProcessAccessorTestCase selector: #testFileProtectionMask).
	suite addTest: (UnixProcessAccessorTestCase selector: #testFileStat).
	suite addTest: (UnixProcessAccessorTestCase selector: #testIsExecutable).
	suite addTest: (UnixProcessAccessorTestCase selector: #testIsExecutableForUserInGroup).
	suite addTest: (UnixProcessAccessorTestCase selector: #testIsReadable).
	suite addTest: (UnixProcessAccessorTestCase selector: #testIsReadableForUserInGroup).
	suite addTest: (UnixProcessAccessorTestCase selector: #testIsWritable).
	suite addTest: (UnixProcessAccessorTestCase selector: #testIsWritableForUserInGroup).

	suite addTest: (UnixProcessAccessorTestCase selector: #runExternalProcessAccess).
	suite addTest: (UnixProcessAccessorTestCase selector: #runForkAndExec).

	result _ suite run.
	self should: [result defects size == 0].
	^ result
! !

!UnixProcessAccessorTestCase methodsFor: 'running' stamp: 'dtl 8/3/2003 18:02'!
runExternalProcessAccess

	"UnixProcessAccessorTestCase new runExternalProcessAccess"

	| result suite |
	suite _ TestSuite new.
	suite addTest: (UnixProcessAccessorTestCase selector: #testCanAccessSystem).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimGetCurrentWorkingDirectory).
	suite addTest: (UnixProcessAccessorTestCase selector: #testChDir).
	suite addTest: (UnixProcessAccessorTestCase selector: #testEnvironmentAt).
	suite addTest: (UnixProcessAccessorTestCase selector: #testEnvironmentAtPut1).
	suite addTest: (UnixProcessAccessorTestCase selector: #testEnvironmentAtPut2).
	suite addTest: (UnixProcessAccessorTestCase selector: #testEnvironmentAtPut).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimUnsetEnv).
	suite addTest: (UnixProcessAccessorTestCase selector: #testGetStdInHandle).
	suite addTest: (UnixProcessAccessorTestCase selector: #testGetStdOutHandle).
	suite addTest: (UnixProcessAccessorTestCase selector: #testGetStdErrHandle).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimArgumentAt).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimErrorMessageAt).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimGetPid).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimGetGid).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimGetEGid).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimGetPid).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimGetPPid).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimGetUid).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimGetSession).
	suite addTest: (UnixProcessAccessorTestCase selector: #testRealpath).

	result _ suite run.
	self should: [result defects size == 0].
	^ result
! !

!UnixProcessAccessorTestCase methodsFor: 'running' stamp: 'dtl 10/6/2001 11:37'!
runForkAndExec
	"Most of this must be tested from class UnixProcess"

	"UnixProcessAccessorTestCase new runForkAndExec"

	| result suite |
	suite _ TestSuite new.
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimGetChildExitStatus).
	suite addTest: (UnixProcessAccessorTestCase selector: #testPrimForkAndExec).
	result _ suite run.
	self should: [result defects size == 0].
	^ result
! !

!UnixProcessAccessorTestCase methodsFor: 'running' stamp: 'dtl 10/6/2001 08:29'!
setUp

	accessor _ ThisOSProcess accessor! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 10/6/2001 08:36'!
testCanAccessSystem

	"(UnixProcessAccessorTestCase selector: #testCanAccessSystem) run"

	self assert: accessor canAccessSystem! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 12/26/2023 19:19'!
testDupTo
	"Perform a dup2() call to assign IO to a new file stream"

	"(UnixProcessAccessorTestCase selector: #testDupTo) run"

	| f1 f2 fd1 fd2 result contents1 contents2 |
	OSProcess deleteFileNamed: '/tmp/delete-1.me'.	
	OSProcess deleteFileNamed: '/tmp/delete-2.me'.	
	f1 _ OSProcess newFileNamed: '/tmp/delete-1.me'.
	f2 _ OSProcess newFileNamed: '/tmp/delete-2.me'.
	fd1 _ OSProcess accessor primUnixFileNumber: f1 fileID.
	fd2 _ OSProcess accessor primUnixFileNumber: f2 fileID.
	result _ OSProcess accessor primDup: fd1 to: fd2.
	"f1 and f2 now both refer to the same IO stream"
	self assert: result = fd2.
	f2 nextPutAll: 'write something to the original f2 file stream that will appear in duped f1 instead'; flush.
	f1 reset.
	contents1 _ f1 upToEnd.
	"Note, the f1 file stream is not aware that we have changed the underlying file descriptor.
	In earlier versions of Squeak and/or Linux stdio runtimes, the contents of f1 matched
	that of f2, and in more recent versions the f1 content appear empty even though the
	underlying file has been written due to the dup2() call. The difference is not important
	for purposes of this test, so the assertions below are commented out. dtl Dec 2023"
	"self deny: contents1 isEmpty.
	self assert: ('write something*' match: contents1)."
	f2 reset.
	contents2 _ f2 upToEnd.
	self deny: contents2 isEmpty.
	self assert: ('write something*' match: contents2).
	f1 close.
	f2 close.
	"check the contents of the actual files"
	contents1 _ (OSProcess fileNamed: '/tmp/delete-1.me') contentsOfEntireFile.
	self assert: ('write something*' match: contents1).
	contents2 _ (OSProcess fileNamed: '/tmp/delete-2.me') contentsOfEntireFile.
	self assert: contents2 isEmpty.
	OSProcess deleteFileNamed: '/tmp/delete-1.me'.	
	OSProcess deleteFileNamed: '/tmp/delete-2.me'.	
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 10/6/2001 11:59'!
testFileProtectionMask

	"(UnixProcessAccessorTestCase selector: #testFileProtectionMask) run"

	| mask |
	mask _ accessor fileProtectionMask: '/bin/rm'.
	self assert: (mask isKindOf: Array).
	self assert: (mask size == 4)
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 10/6/2001 12:06'!
testFileStat

	"(UnixProcessAccessorTestCase selector: #testFileStat) run"

	| stat |
	stat _ accessor fileStat: '/bin/rm'.
	self assert: (stat isKindOf: Array).
	self assert: (stat size == 3).
	self assert: ((stat at: 1) isKindOf: Integer).
	self assert: ((stat at: 2) isKindOf: Integer).
	self assert: ((stat at: 3) isKindOf: Array).
	self assert: ((stat at: 3) size == 4)
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 10/6/2001 12:08'!
testIsExecutable

	"(UnixProcessAccessorTestCase selector: #testIsExecutable) run"

	self should: [accessor isExecutable: '/bin/sh'].
	self shouldnt: [accessor isExecutable: '/etc/hosts']
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 1/23/2013 21:35'!
testIsExecutableForUserInGroup

	"(UnixProcessAccessorTestCase selector: #testIsExecutableForUserInGroup) run"

	self shouldnt: [OSProcess waitForCommand: 'touch /tmp/delete.me']
		raise: Warning.
	OSProcess waitForCommand: 'chmod 550 /tmp/delete.me'.
	"explicitly set file group because OS X may have set it to something other than current gid"
	OSProcess waitForCommand: 'chgrp ', accessor primGetGid asString, ' /tmp/delete.me'.
	self should: [accessor isExecutable: '/bin/sh'
					forUser: accessor primGetUid
					inGroup: accessor primGetGid].
	self shouldnt: [accessor isExecutable: '/etc/hosts'
					forUser: accessor primGetUid
					inGroup: accessor primGetGid].
	self should: [accessor isExecutable: '/tmp/delete.me'
					forUser: accessor primGetUid
					inGroup: 99999].
	self should: [accessor isExecutable: '/tmp/delete.me'
					forUser: 99999
					inGroup: accessor primGetGid].
	self shouldnt: [accessor isExecutable: '/tmp/delete.me'
					forUser: 99999
					inGroup: 99999].
	OSProcess deleteFileNamed: '/tmp/delete.me'
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 10/20/2001 11:39'!
testIsReadable

	"(UnixProcessAccessorTestCase selector: #testIsReadable) run"

	self should: [accessor isReadable: '/bin/sh'].
	OSProcess waitForCommand: 'touch /tmp/delete.me'.
	OSProcess waitForCommand: 'chmod 440 /tmp/delete.me'.
	(Delay forMilliseconds: 200) wait.
	self should: [accessor isReadable: '/tmp/delete.me'].
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 1/23/2013 21:35'!
testIsReadableForUserInGroup

	"(UnixProcessAccessorTestCase selector: #testIsReadableForUserInGroup) run"

	self shouldnt: [OSProcess waitForCommand: 'touch /tmp/delete.me']
		raise: Warning.
	OSProcess waitForCommand: 'chmod 550 /tmp/delete.me'.
	"explicitly set file group because OS X may have set it to something other than current gid"
	OSProcess waitForCommand: 'chgrp ', accessor primGetGid asString, ' /tmp/delete.me'.
	self should: [accessor isReadable: '/bin/sh'
					forUser: accessor primGetUid
					inGroup: accessor primGetGid].
	self should: [accessor isReadable: '/tmp/delete.me'
					forUser: accessor primGetUid
					inGroup: 99999].
	self should: [accessor isReadable: '/tmp/delete.me'
					forUser: 99999
					inGroup: accessor primGetGid].
	self shouldnt: [accessor isReadable: '/tmp/delete.me'
					forUser: 99999
					inGroup: 99999].
	OSProcess deleteFileNamed: '/tmp/delete.me'
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 10/20/2001 11:38'!
testIsWritable

	"(UnixProcessAccessorTestCase selector: #testIsWritable) run"

	self shouldnt: [accessor isWritable: '/bin/sh'].
	OSProcess waitForCommand: 'touch /tmp/delete.me'.
	OSProcess waitForCommand: 'chmod 770 /tmp/delete.me'.
	(Delay forMilliseconds: 200) wait.
	self should: [accessor isWritable: '/tmp/delete.me'].
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 1/23/2013 21:35'!
testIsWritableForUserInGroup

	"(UnixProcessAccessorTestCase selector: #testIsWritableForUserInGroup) run"

	self shouldnt: [OSProcess waitForCommand: 'touch /tmp/delete.me']
		raise: Warning.
	OSProcess waitForCommand: 'chmod 770 /tmp/delete.me'.
	"explicitly set file group because OS X may have set it to something other than current gid"
	OSProcess waitForCommand: 'chgrp ', accessor primGetGid asString, ' /tmp/delete.me'.
	self shouldnt: [accessor isWritable: '/bin/sh'
					forUser: accessor primGetUid
					inGroup: accessor primGetGid].
	self should: [accessor isWritable: '/tmp/delete.me'
					forUser: accessor primGetUid
					inGroup: 99999].
	self should: [accessor isWritable: '/tmp/delete.me'
					forUser: 99999
					inGroup: accessor primGetGid].
	self shouldnt: [accessor isWritable: '/tmp/delete.me'
					forUser: 99999
					inGroup: 99999].
	OSProcess deleteFileNamed: '/tmp/delete.me'
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 10/6/2001 11:48'!
testMakePipeHandles

	"(UnixProcessAccessorTestCase selector: #testMakePipeHandles) run"

	| p |
	p _ OSPipe new.
	self assert: p reader closed not.
	self assert: p writer closed not.
	p close! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 12/31/2023 11:58'!
testRedirectStdOutTo
	"Perform a dup2() call on the standard output descriptor to assign it
	to a new file stream"

	"(UnixProcessAccessorTestCase selector: #testRedirectStdOutTo) run"

	| out aFileStream contents outFd saveFd newFileNo saveFs |
	out _ OSProcess thisOSProcess stdOut.
	"save file number to allow cleanup at end of test"
	OSProcess deleteFileNamed: '/tmp/delete.2.me'.	
	saveFs _ OSProcess newFileNamed: '/tmp/delete.2.me'.
	outFd _ OSProcess accessor primUnixFileNumber: out fileID.
	saveFd _ OSProcess accessor primUnixFileNumber: saveFs fileID.
	OSProcess accessor primDup: outFd to: saveFd.
	"Original stdout file descriptor is saved as safeFd, to be restored at end of test"

	OSProcess deleteFileNamed: '/tmp/delete.me'.	
	aFileStream _ OSProcess newFileNamed: '/tmp/delete.me'.
	"nb - #contentsOfEntireFile closes the file, do not use"
	aFileStream reset.
	self assert: aFileStream upToEnd isEmpty.
	"note - do not close aFileStream as this affects actual stdout"
	out nextPutAll: 'foo'; flush.
	aFileStream reset.
	self assert: aFileStream upToEnd isEmpty.
	OSProcess thisOSProcess redirectStdOutTo: aFileStream.
	out nextPutAll: 'foo'; flush.
	contents _ self contentsOfFile: '/tmp/delete.me'.
	self deny: contents isEmpty.
	self assert: contents = 'foo'. "assume noone else is using stdout right now"
	"ensure the we can still find the real stdout"
	self deny: OSProcess thisOSProcess stdOut closed.
	OSProcess thisOSProcess stdOut nextPutAll: 'bar'; flush.
	contents _ self contentsOfFile: '/tmp/delete.me'.
	self deny: contents isEmpty.
	self assert: (contents endsWith: 'bar').
	"Restore the original file number. The stdOut stream is cached in ThisOSProcess
	(and should probably be recalculated following a dup2(), so find the real handle
	with getStdOutHandle."
	newFileNo _ OSProcess accessor primUnixFileNumber: OSProcess accessor getStdOutHandle.
	OSProcess accessor primDup: saveFd to: newFileNo.
	OSProcess thisOSProcess stdOut lf; nextPutAll: 'UnixProcessAccessorTestCase>>testRedirectStdOutTo - this should appear on original console stdout'; lf.
	saveFs close.
	aFileStream close.	
	OSProcess deleteFileNamed: '/tmp/delete.me'.	
	OSProcess deleteFileNamed: '/tmp/delete.2.me'.	

! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 1/19/2021 21:32'!
testSessionIdentifier
	"This is an indirect way to make sure that the primitive for obtaining the session ID does
	not get out of kilter with respect to the method for obtaining session ID from an existing
	open file. The dangerous failure mode is if the SQFile data stucture format changes and
	OSProcess does not get updated to reflect the change.

	As of approximately Squeak 3.8 and beyond, the session id has been moved to the
	first slot of the data structure. Deducing the session ID from an open file will not
	work for a Squeak VM beyond that point, and will not work for any 64 bit VM.
	OSPP version 4.0 or higher obtains session ID from the interpreter, so this is no
	longer an issue. Skip this test for OSPP 4.0 and higher."

	"(UnixProcessAccessorTestCase selector: #testSessionIdentifier) debug"

	| installedOSPPMajorVersion |
	self should: [accessor getSessionIdentifier isKindOf: ByteArray].
	installedOSPPMajorVersion _ Integer readFrom:
		(OSProcess accessor osppModuleVersionString ifNil: ['-1']) readStream.
	(installedOSPPMajorVersion >= 4)
		ifTrue: ["ignore this test"]
		ifFalse: [self should:
			[accessor getSessionIdentifierFromSourcesFile = accessor getSessionIdentifier]]
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'eem 3/27/2014 11:32'!
testSignalNumbers
	"Signal numbers as reported by UnixOSProcessPlugin. Note that some signal definitions
	are architecture dependent, with specific number assignments differing between e.g. Intel
	and Sparc. Intel architecture is assumed for this test."

	"NOTE OSPP prior to version 4.4.12 had several incorrect signal number assignments.
	Check the version level of OSPP by evaluating this expression:

		OSProcess accessor primOSProcessPluginModuleVersionString"

	self assert: OSProcess accessor primSigHupNumber = 1.
	self assert: OSProcess accessor primSigIntNumber = 2.
	self assert: OSProcess accessor primSigKillNumber = 9.
	self assert: OSProcess accessor primSigPipeNumber = 13.
	self assert: OSProcess accessor primSigQuitNumber = 3.
	self assert: OSProcess accessor primSigTermNumber = 15.
	OSProcess platformName caseOf:
		{ ['unix']		->
			[self assert: OSProcess accessor primSigUsr1Number = 10.
			 self assert: OSProcess accessor primSigUsr2Number = 12.
			 self assert: OSProcess accessor primSigChldNumber = 17.].
		   ['Mac OS']	->
			[self assert: OSProcess accessor primSigUsr1Number = 30.
			 self assert: OSProcess accessor primSigUsr2Number = 31.
			 self assert: OSProcess accessor primSigChldNumber = 20.]}
	
	
! !

!UnixProcessAccessorTestCase methodsFor: 'testing' stamp: 'dtl 1/15/2018 21:59:06'!
testUnixFileNumber
	"For purposes of this test, assume that stdin and stderr have not been
	directed."

	"(UnixProcessAccessorTestCase selector: #testUnixFileNumber) run"

	| err errFileNo errHandle in inFileNo inHandle aFileStream fFileNo fHandle |
	err _ OSProcess thisOSProcess stdErr.
	errHandle _ err fileID.
	errFileNo _ OSProcess accessor unixFileNumber: errHandle.
	self assert: errFileNo == 2.
	in _ OSProcess thisOSProcess stdIn.
	inHandle _ in fileID.
	inFileNo _ OSProcess accessor unixFileNumber: inHandle.
	self assert: inFileNo == 0.
	aFileStream _ OSProcess fileNamed: '/tmp/delete.me'.
	fHandle _ aFileStream fileID.
	fFileNo _ OSProcess accessor unixFileNumber: fHandle.
	self assert: fFileNo notNil.
	self assert: fFileNo > 2.
	aFileStream close

! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:57:58'!
testIsLockableFile

	"(UnixProcessAccessorTestCase selector: #testIsLockableFile) debug"

	| fs result |
	fs _ OSProcess fileNamed: 'junkfile'.
	[fs nextPutAll: 'ABCDEFG'.

	result _ OSProcess accessor isLockableFile: fs.
	self should: result.

	result _ OSProcess accessor lockFile: fs.
	self should: (result isKindOf: OSFileLock).
	result _ OSProcess accessor isLockableFile: fs.
	self should: result.

	OSProcess accessor unlockFile: fs.
	result _ OSProcess accessor isLockableFile: fs.
	self should: result.

	fs close.
	result _ OSProcess accessor isLockableFile: fs.
	self shouldnt: result.
	] ensure: [fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:58:08'!
testLockFile

	"(UnixProcessAccessorTestCase selector: #testLockFile) debug"

	| fs result |
	fs _ OSProcess fileNamed: 'junkfile'.
	[fs nextPutAll: 'ABCDEFG'.
	result _ OSProcess accessor lockFile: fs.
	self should: (result isKindOf: OSFileLock).
	fs close.
	result _ OSProcess accessor lockFile: fs.
	self should: (result == nil).
	] ensure: [fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:58:17'!
testOSFileLockLock

	"(UnixProcessAccessorTestCase selector: #testOSFileLockLock) debug"

	| fs fileLock result |
	fs _ OSProcess fileNamed: 'junkfile'.
	[fs nextPutAll: 'ABCDEFG'.
	fileLock _ OSFileLock onFile: fs exclusive: true.
	result _ fileLock lock.
	self should: (result == true).
	result _ fileLock lock.
	self should: (result == true).
	fs close.
	result _ fileLock lock.
	self should: (result == false).
	] ensure: [fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:58:24'!
testOSFileLockTest

	"(UnixProcessAccessorTestCase selector: #testOSFileLockTest) debug"

	| fs fileLock result |
	fs _ OSProcess fileNamed: 'junkfile'.
	[fs nextPutAll: 'ABCDEFG'.
	fileLock _ OSFileLock onFile: fs exclusive: true.
	result _ fileLock test.
	self should: (result == true).
	result _ fileLock lock.
	self should: (result == true).
	result _ fileLock test.
	self should: (result == true).
	result _ fileLock unlock.
	self should: (result == true).
	result _ fileLock test.
	self should: (result == true).
	fs close.
	result _ fileLock test.
	self should: (result == false).
	] ensure: [fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:58:33'!
testOSFileLockUnlock

	"(UnixProcessAccessorTestCase selector: #testOSFileLockUnlock) debug"

	| fs fileLock result |
	fs _ OSProcess fileNamed: 'junkfile'.
	[fs nextPutAll: 'ABCDEFG'.
	fileLock _ OSFileLock onFile: fs exclusive: true.
	result _ fileLock unlock.
	self should: (result == true).
	result _ fileLock lock.
	self should: (result == true).
	result _ fileLock unlock.
	self should: (result == true).
	fs close.
	result _ fileLock unlock.
	self should: (result == false).
	] ensure: [fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:58:41'!
testOSFileRegionLockLock

	"(UnixProcessAccessorTestCase selector: #testOSFileRegionLockLock) debug"

	| fs fileLock result |
	fs _ OSProcess fileNamed: 'junkfile'.
	[fs nextPutAll: 'ABCDEFG'.
	fileLock _ OSFileRegionLock onFile: fs from: 2 to: 4 exclusive: true.
	result _ fileLock lock.
	self should: (result == true).
	result _ fileLock lock.
	self should: (result == true).
	fs close.
	result _ fileLock lock.
	self should: (result == false).
	] ensure: [fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:58:49'!
testOSFileRegionLockTest

	"(UnixProcessAccessorTestCase selector: #testOSFileRegionLockTest) debug"

	| fs fileLock result |
	fs _ OSProcess fileNamed: 'junkfile'.
	[fs nextPutAll: 'ABCDEFG'.
	fileLock _ OSFileRegionLock onFile: fs from: 2 to: 4 exclusive: true.
	result _ fileLock test.
	self should: (result == true).
	result _ fileLock lock.
	self should: (result == true).
	result _ fileLock test.
	self should: (result == true).
	result _ fileLock unlock.
	self should: (result == true).
	result _ fileLock test.
	self should: (result == true).
	fs close.
	result _ fileLock test.
	self should: (result == false).
	] ensure: [fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:58:57'!
testOSFileRegionLockUnlock

	"(UnixProcessAccessorTestCase selector: #testOSFileRegionLockUnlock) debug"

	| fs fileLock result |
	fs _ OSProcess fileNamed: 'junkfile'.
	[fs nextPutAll: 'ABCDEFG'.
	fileLock _ OSFileRegionLock onFile: fs from: 2 to: 4 exclusive: true.
	result _ fileLock unlock.
	self should: (result == true).
	result _ fileLock lock.
	self should: (result == true).
	result _ fileLock unlock.
	self should: (result == true).
	fs close.
	result _ fileLock unlock.
	self should: (result == false).
	] ensure: [fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:59:17'!
testUnlockFileUnixCompatible

	"(UnixProcessAccessorTestCase selector: #testUnlockFileUnixCompatible) debug"

	| mode fs result |
	mode _ OSProcess accessor emulateWin32FileLocking. "remember setting"

	fs _ OSProcess fileNamed: 'junkfile'.
	[OSProcessAccessor emulateWin32FileLocking: false. "unix mode"
	fs nextPutAll: 'ABCDEFG'.
	result _ OSProcess accessor unlockFile: fs.
	"On Unix, the fcntl call to unlock the file will succeed even if
	the file is not locked."
	self should: (result isKindOf: OSFileLock).

	result _ OSProcess accessor lockFile: fs.
	self should: (result isKindOf: OSFileLock).
	result _ OSProcess accessor unlockFile: fs.
	self should: (result isKindOf: OSFileLock).

	fs close.
	result _ OSProcess accessor unlockFile: fs.
	self should: result == nil] ensure:
		[OSProcessAccessor emulateWin32FileLocking: mode.
		fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'file locking' stamp: 'dtl 1/15/2018 21:59:24'!
testUnlockFileWin32Compatible

	"(UnixProcessAccessorTestCase selector: #testUnlockFileWin32Compatible) debug"

	| mode fs result |
	mode _ OSProcess accessor emulateWin32FileLocking. "remember setting"

	fs _ OSProcess fileNamed: 'junkfile'.
	[OSProcessAccessor emulateWin32FileLocking: true. "win32 mode"
	fs nextPutAll: 'ABCDEFG'.
	result _ OSProcess accessor unlockFile: fs.
	self should: result == nil.

	result _ OSProcess accessor lockFile: fs.
	self should: (result isKindOf: OSFileLock).
	result _ OSProcess accessor unlockFile: fs.
	self should: (result isKindOf: OSFileLock).

	fs close.
	result _ OSProcess accessor unlockFile: fs.
	self should: result == nil] ensure:
		[OSProcessAccessor emulateWin32FileLocking: mode.
		fs close]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - C string arrays' stamp: 'dtl 7/11/2005 19:00'!
primFixPointersInArrayOfStrings: cStringArray withOffsets: offsetArray count: count
	"This primitive call exists only for purposes of testing the
	fixPointersInArrayOfStrings:withOffsets:count: method. I believe it to be
	reasonably machine and compiler independent, but have no way of verifying
	this on a variety of machines, so I'll leave this test method here in case
	someone runs into problems on other hardware or compilers. -dtl"

	"| a |
	a _ OSProcess thisOSProcess envAsFlatArrayAndOffsets: UnixProcess env.
	UnixProcessAccessorTestCase new
		primFixPointersInArrayOfStrings: (a at: 1)
		withOffsets: (a at: 2)
		count: (a at: 2) size"

	<primitive: 'primitiveFixPointersInArrayOfStrings' module: 'UnixOSProcessPlugin'>
	^ nil! !

!UnixProcessAccessorTestCase methodsFor: 'tests - C string arrays' stamp: 'dtl 12/18/2022 10:56'!
testArgsAsFlatArrayAndOffsets
	"Test conversion of an array of strings into the form used to create a C **char
	in UnixOSProcessPlugin."

	"(UnixProcessAccessorTestCase selector: #testArgsAsFlatArrayAndOffsets) debug"

	| a pointerSize lengthOfCStrings totalLength stringsAndOffsets rs s arrayWithMachineDependentPointers |
	pointerSize _ OSProcess accessor sizeOfPointer.
	a _ { 'one' . 'two' . 'three' } collect: [ :e | e , (Character null) asString ].
	stringsAndOffsets _ OSProcess thisOSProcess argsAsFlatArrayAndOffsets: a.
	self assert: stringsAndOffsets size == 2.
	self assert: stringsAndOffsets second size == 3.
	lengthOfCStrings _ (a collect: [:e | e size]) sum.
	totalLength _ 4 * pointerSize + lengthOfCStrings.
	self assert: stringsAndOffsets first size == totalLength.
	stringsAndOffsets second do: [:offset |
		(stringsAndOffsets first at: offset) == (Character null)].

	rs _ ReadStream on: stringsAndOffsets first.
	rs next: 4 * pointerSize.
	s _  rs next: 3.
	rs next.
	self assert: s = 'one' asByteArray.
	s _  rs next: 3.
	rs next.
	self assert: s = 'two' asByteArray.
	s _  rs next: 5.
	rs next.
	self assert: s = 'three' asByteArray.

	rs _ ReadStream on: stringsAndOffsets first.
	rs next: (stringsAndOffsets second at: 1).
	s _  rs next: 3.
	self assert: s = 'one' asByteArray.
	rs _ ReadStream on: stringsAndOffsets first.
	rs next: (stringsAndOffsets second at: 2).
	s _  rs next: 3.
	self assert: s = 'two' asByteArray.
	rs _ ReadStream on: stringsAndOffsets first.
	rs next: (stringsAndOffsets second at: 3).
	s _  rs next: 5.
	self assert: s = 'three' asByteArray.

	arrayWithMachineDependentPointers _ self
		primFixPointersInArrayOfStrings: stringsAndOffsets first
		withOffsets: stringsAndOffsets second
		count: stringsAndOffsets second size.
	self assert: arrayWithMachineDependentPointers size == stringsAndOffsets first size.
	rs _ ReadStream on: arrayWithMachineDependentPointers.
	s _ rs next: pointerSize.
	self assert: s ~= (String new: pointerSize). "a C pointer"
	s _ rs next: pointerSize.
	self assert: s ~= (String new: pointerSize). "a C pointer"
	s _ rs next: pointerSize.
	self assert: s ~= (String new: pointerSize). "a C pointer"
	s _ rs next: pointerSize.
	self assert: s = (String new: pointerSize) asByteArray. "a NULL pointer"
	self assert: rs upToEnd = (stringsAndOffsets first allButFirst: pointerSize * 4)
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - C string arrays' stamp: 'dtl 12/18/2022 11:14'!
testEnvAsFlatArrayAndOffsets
	"Test conversion of a dictionary of key-value environment variables into the
	form used to create a C **char in UnixOSProcessPlugin."

	"(UnixProcessAccessorTestCase selector: #testEnvAsFlatArrayAndOffsets) debug"

	| pointerSize lengthOfCStrings totalLength stringsAndOffsets rs s arrayWithMachineDependentPointers d |
	pointerSize _ OSProcess accessor sizeOfPointer.
	d _ Dictionary new.
	d at: #ONE put: 'one'.
	d at: #TWO put: 'two'.
	d at: #THREE put: 'three'.
	stringsAndOffsets _ OSProcess thisOSProcess envAsFlatArrayAndOffsets: d.
	self assert: stringsAndOffsets size == 2.
	self assert: stringsAndOffsets second size == 3.

	lengthOfCStrings _ d keys
		inject: 0
		into: [:sum :e | sum + e size + '=' size + (d at: e) size + (String new: 1) size].

	totalLength _ 4 * pointerSize + lengthOfCStrings.
	self assert: stringsAndOffsets first size == totalLength.
	stringsAndOffsets second do: [:offset |
		(stringsAndOffsets first at: offset) == (Character null)].

	arrayWithMachineDependentPointers _ self
		primFixPointersInArrayOfStrings: stringsAndOffsets first
		withOffsets: stringsAndOffsets second
		count: stringsAndOffsets second size.
	self assert: arrayWithMachineDependentPointers size == stringsAndOffsets first size.
	rs _ ReadStream on: arrayWithMachineDependentPointers.
	s _ rs next: pointerSize.
	self assert: s ~= (String new: pointerSize). "a C pointer"
	s _ rs next: pointerSize.
	self assert: s ~= (String new: pointerSize). "a C pointer"
	s _ rs next: pointerSize.
	self assert: s ~= (String new: pointerSize). "a C pointer"
	s _ rs next: pointerSize.
	self assert: s = (String new: pointerSize) asByteArray. "a NULL pointer"
	self assert: rs upToEnd = (stringsAndOffsets first allButFirst: pointerSize * 4)
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 12/26/2023 11:28'!
testChDir

	"(UnixProcessAccessorTestCase selector: #testChDir) run"

	| cwd new |
	cwd _ accessor primGetCurrentWorkingDirectory.
	new _ '/usr/bin'.
	"Note, do not use /tmp because OS X implements it as a sym link to private/tmp
	and do not use /bin because some Linux implementations may link it to /usr/bin"
	accessor chDir: new.
	self should: [new = accessor primGetCurrentWorkingDirectory].
	accessor chDir: cwd.
	self should: [cwd = accessor primGetCurrentWorkingDirectory].
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 08:53'!
testEnvironmentAt

	"(UnixProcessAccessorTestCase selector: #testEnvironmentAt) run"

	| path |
	path _ accessor environmentAt: 'PATH'.
	self should: [path notNil and: [path isEmpty not]]! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 2/24/2013 10:26'!
testEnvironmentAtPut

	"(UnixProcessAccessorTestCase selector: #testEnvironmentAtPut) run"

	| oldPath newPath resetPath s |
	oldPath _ accessor environmentAt: 'PATH'.
	newPath _ 'this is a test string'.
	accessor environmentAt: 'PATH' put: newPath.
	resetPath _ accessor environmentAt: 'PATH'.
	self should: [newPath = resetPath].
	accessor environmentAt: 'PATH' put: oldPath.
	resetPath _ accessor environmentAt: 'PATH'.
	self should: [oldPath = resetPath].
	s _ WriteStream on: String new.
	10000 timesRepeat: [s nextPutAll: 'Do something to provoke garbage collection'].
	resetPath _ accessor environmentAt: 'PATH'.
	self should: [oldPath = resetPath].


! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/7/2001 11:47'!
testEnvironmentAtPut1

	"(UnixProcessAccessorTestCase selector: #testEnvironmentAtPut1) run"

	| oldVal newVal resetVal |
	oldVal _ accessor environmentAt: 'AAAA'.
	newVal _ 'this is a test string'.
	accessor environmentAt: 'AAAA' put: newVal.
	resetVal _ accessor environmentAt: 'AAAA'.
	self should: [resetVal notNil and: [resetVal isEmpty not]].
	self should: [newVal = resetVal].
	accessor environmentAt: 'AAAA' put: oldVal.
	self should: [oldVal = (accessor environmentAt: 'AAAA')]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/7/2001 12:14'!
testEnvironmentAtPut2
	"This looks for a bug in which the enviroment is successfully, but later gets
	invalidated by memory moves or reallocation."

	"(UnixProcessAccessorTestCase selector: #testEnvironmentAtPut2) run"

	| oldVal newVal resetVal count ws goodResults notFound totallyBogus |
	oldVal _ accessor environmentAt: 'AAAA'.
	newVal _ 'this is a test string'.
	accessor environmentAt: 'AAAA' put: newVal.
	resetVal _ accessor environmentAt: 'AAAA'.
	self should: [resetVal notNil and: [resetVal isEmpty not]].
	self should: [newVal = resetVal].

	count _ 50000.
	ws _ WriteStream on: Array new.
	count timesRepeat: [ws nextPut: (accessor environmentAt: 'AAAA')].
	goodResults _ ws contents select: [:e | e = newVal].
	notFound _ ws contents select: [:e | e isNil].
	totallyBogus _ ws contents select: [:e | e notNil and: [e ~= newVal]].

	"Uncomment the following to see when the problem is happening"
	"(Array with: goodResults with: notFound with: totallyBogus) inspect."

	self should: [goodResults size == count].
	self should: [notFound isEmpty].
	self should: [totallyBogus isEmpty].

	accessor environmentAt: 'AAAA' put: oldVal.
	self should: [oldVal = (accessor environmentAt: 'AAAA')]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:36'!
testGetStdErrHandle

	"(UnixProcessAccessorTestCase selector: #testGetStdErrHandle) run"

	| handle |
	handle _ accessor primGetStdErrHandle.
	self assert: handle notNil
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:36'!
testGetStdInHandle

	"(UnixProcessAccessorTestCase selector: #testGetStdInHandle) run"

	| handle |
	handle _ accessor primGetStdInHandle.
	self assert: handle notNil
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:36'!
testGetStdOutHandle

	"(UnixProcessAccessorTestCase selector: #testGetStdOutHandle) run"

	| handle |
	handle _ accessor primGetStdOutHandle.
	self assert: handle notNil
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:42'!
testPrimArgumentAt

	"(UnixProcessAccessorTestCase selector: #testPrimArgumentAt) run"

	| progName |
	progName _ accessor primArgumentAt: 1.
	self should: [progName notNil and: [progName isEmpty not]]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:45'!
testPrimErrorMessageAt

	"(UnixProcessAccessorTestCase selector: #testPrimErrorMessageAt) run"

	| msg |
	msg _ accessor primErrorMessageAt: 1.
	self should: [msg notNil and: [msg isEmpty not]].
	msg _ accessor primErrorMessageAt: 0.
	self should: [msg notNil and: [msg isEmpty not]]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 08:45'!
testPrimGetCurrentWorkingDirectory

	"(UnixProcessAccessorTestCase selector: #testPrimGetCurrentWorkingDirectory) run"

	| cwd |
	cwd _ accessor primGetCurrentWorkingDirectory.
	self should: [cwd notNil and: [cwd isEmpty not]]! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:49'!
testPrimGetEGid

	"(UnixProcessAccessorTestCase selector: #testPrimGetEGid) run"

	| id |
	id _ accessor primGetEGid.
	self should: [id isKindOf: Integer]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:49'!
testPrimGetEUid

	"(UnixProcessAccessorTestCase selector: #testPrimGetEUid) run"

	| id |
	id _ accessor primGetEUid.
	self should: [id isKindOf: Integer]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:50'!
testPrimGetGid

	"(UnixProcessAccessorTestCase selector: #testPrimGetGid) run"

	| id |
	id _ accessor primGetGid.
	self should: [id isKindOf: Integer]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:50'!
testPrimGetPPid

	"(UnixProcessAccessorTestCase selector: #testPrimGetPPid) run"

	| id |
	id _ accessor primGetPPid.
	self should: [id isKindOf: Integer]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:50'!
testPrimGetPid

	"(UnixProcessAccessorTestCase selector: #testPrimGetPid) run"

	| id |
	id _ accessor primGetPid.
	self should: [id isKindOf: Integer]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 8/19/2005 06:32'!
testPrimGetSession
	"As of OSPP version 4 and greater, the session ID should always be obtained from
	the interpreter. Previously it was deduced by looking at the handle of an open file."

	"(UnixProcessAccessorTestCase selector: #testPrimGetSession) run"

	| id expectedSessionSize |
	expectedSessionSize _ [OSProcess accessor sizeOfInt] on: Error do: [4].
	id _ OSProcess accessor primGetSession.
	self assert: (id isNil or: [id isKindOf: ByteArray]).
	id _ OSProcess accessor getSessionIdentifier.
	self assert: (id isKindOf: ByteArray).
	self assert: (id size / expectedSessionSize) == 1

! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 09:50'!
testPrimGetUid

	"(UnixProcessAccessorTestCase selector: #testPrimGetUid) run"

	| id |
	id _ accessor primGetUid.
	self should: [id isKindOf: Integer]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/7/2001 14:07'!
testPrimUnsetEnv

	"(UnixProcessAccessorTestCase selector: #testPrimUnsetEnv) run"

	| oldVal newVal resetVal |
	oldVal _ accessor environmentAt: 'AAAA'.
	newVal _ 'this is a test string'.
	accessor environmentAt: 'AAAA' put: newVal.
	resetVal _ accessor environmentAt: 'AAAA'.
	self should: [resetVal notNil and: [resetVal isEmpty not]].
	self should: [newVal = resetVal].

	accessor primUnsetEnv: 'AAAA', (Character null) asString.
	resetVal _ accessor environmentAt: 'AAAA'.
	self should: [resetVal isNil].

	accessor environmentAt: 'AAAA' put: oldVal.
	self should: [oldVal = (accessor environmentAt: 'AAAA')]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - external process access' stamp: 'dtl 10/6/2001 10:03'!
testRealpath

	"(UnixProcessAccessorTestCase selector: #testRealpath) run"

	| p |
	p _ accessor realpath: '/usr/bin'.
	self should: [p notNil and: [p isEmpty not]].
	p _ accessor realpath: '/bogus/path/name'.
	self should: [p isNil]
! !

!UnixProcessAccessorTestCase methodsFor: 'tests - fork and exec' stamp: 'dtl 6/11/2011 12:25'!
testPrimForkAndExec
	"These methods can only be tested properly from UnixProcess, which knows how
	to set up the parameters on the stack."

	"(UnixProcessAccessorTestCase selector: #testPrimForkAndExec) run"

	| p |
	self shouldnt: [p _ UnixProcess command: 'ls /bin']
		raise: Warning.
	(Delay forMilliseconds: 500) wait.
	self should: [p isComplete].
	self should: [p exitStatus == 0]

! !

!UnixProcessAccessorTestCase methodsFor: 'tests - fork and exec' stamp: 'dtl 10/6/2001 10:44'!
testPrimGetChildExitStatus
	"Cannot really test this here, because it needs to be wired into the interrupt
	handler. Just make sure it returns nil when there is nothing to do."

	"(UnixProcessAccessorTestCase selector: #testPrimGetChildExitStatus) run"

	| stat |
	stat _ accessor primGetChildExitStatus: -1.
	self should: [stat isNil].
	stat _ accessor primGetChildExitStatus: 1.
	self should: [stat isNil]
! !

!UnixProcessFileLockTestCase methodsFor: 'asserting' stamp: 'jf 2/22/2004 19:07'!
assertLock: lock1 lock: lock2 conflicts: aBoolean
	self assert: (lock1 conflictsWith: lock2) = aBoolean.
	self assert: (lock2 conflictsWith: lock1) = aBoolean.! !

!UnixProcessFileLockTestCase methodsFor: 'data' stamp: 'dtl 12/17/2016 17:37'!
entireExclusiveLock
	^ OSFileLock onFile: fileStream exclusive: true! !

!UnixProcessFileLockTestCase methodsFor: 'data' stamp: 'dtl 12/17/2016 17:37'!
entireSharedLock
	^ OSFileLock onFile: fileStream exclusive: false! !

!UnixProcessFileLockTestCase methodsFor: 'data' stamp: 'jf 2/22/2004 19:22'!
regionExclusiveLock
	^ self regionLockFrom: 10 to: 20 exclusive: true! !

!UnixProcessFileLockTestCase methodsFor: 'data' stamp: 'dtl 12/17/2016 17:37'!
regionLockFrom: from to: to exclusive: exclusive
	^ OSFileRegionLock onFile: fileStream from: from to: to exclusive: exclusive! !

!UnixProcessFileLockTestCase methodsFor: 'data' stamp: 'jf 2/22/2004 19:22'!
regionSharedLock
	^ self regionLockFrom: 10 to: 20 exclusive: false! !

!UnixProcessFileLockTestCase methodsFor: 'accessing' stamp: 'jf 2/21/2004 18:14'!
fileStream
	^ fileStream! !

!UnixProcessFileLockTestCase methodsFor: 'running' stamp: 'dtl 1/15/2018 22:18:35'!
setUp
	OSProcess deleteFileNamed: 'junkfile'.
	fileStream _ OSProcess fileNamed: 'junkfile'.! !

!UnixProcessFileLockTestCase methodsFor: 'running' stamp: 'dtl 1/23/2013 19:42'!
tearDown
	fileStream close.
	OSProcess deleteFileNamed: fileStream fullName! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:27'!
testDistinctExclusiveRegionWithExclusiveRegion
	self
		assertLock: (self regionLockFrom: 10 to: 20 exclusive: true)
		lock: (self regionLockFrom: 30 to: 40 exclusive: true)
		conflicts: false! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:27'!
testDistinctExclusiveRegionWithSharedRegion
	self
		assertLock: (self regionLockFrom: 10 to: 20 exclusive: true)
		lock: (self regionLockFrom: 30 to: 40 exclusive: false)
		conflicts: false! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:27'!
testDistinctSharedRegionWithExclusiveRegion
	self
		assertLock: (self regionLockFrom: 10 to: 20 exclusive: false)
		lock: (self regionLockFrom: 30 to: 40 exclusive: true)
		conflicts: false! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:26'!
testDistinctSharedRegionWithSharedRegion
	self
		assertLock: (self regionLockFrom: 10 to: 20 exclusive: false)
		lock: (self regionLockFrom: 30 to: 40 exclusive: false)
		conflicts: false! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:12'!
testEntireExclusiveWithEntireExclusive
	self
		assertLock: self entireExclusiveLock
		lock: self entireExclusiveLock
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:16'!
testEntireExclusiveWithExclusiveRegion
	self
		assertLock: self entireExclusiveLock
		lock: self regionExclusiveLock
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:17'!
testEntireExclusiveWithSharedRegion
	self
		assertLock: self entireExclusiveLock
		lock: self regionSharedLock
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:12'!
testEntireSharedWithEntireExclusive
	self
		assertLock: self entireSharedLock
		lock: self entireExclusiveLock
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:12'!
testEntireSharedWithEntireShared
	self
		assertLock: self entireSharedLock
		lock: self entireSharedLock
		conflicts: false! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:14'!
testEntireSharedWithExclusiveRegion
	self
		assertLock: self entireSharedLock
		lock: self regionExclusiveLock
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:15'!
testEntireSharedWithSharedRegion
	self
		assertLock: self entireSharedLock
		lock: self regionSharedLock
		conflicts: false! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:18'!
testExclusiveRegionWithExclusiveRegion
	self
		assertLock: self regionExclusiveLock
		lock: self regionExclusiveLock
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:25'!
testIdenticalExclusiveRegionWithExclusiveRegion
	self
		assertLock: self regionExclusiveLock
		lock: self regionExclusiveLock
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:24'!
testIdenticalSharedRegionWithExclusiveRegion
	self
		assertLock: self regionSharedLock
		lock: self regionExclusiveLock
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:24'!
testIdenticalSharedRegionWithSharedRegion
	self
		assertLock: self regionSharedLock
		lock: self regionSharedLock
		conflicts: false! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:25'!
testOverlappingExclusiveRegionWithExclusiveRegion
	self
		assertLock: (self regionLockFrom: 10 to: 20 exclusive: true)
		lock: (self regionLockFrom: 20 to: 30 exclusive: true)
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:26'!
testOverlappingExclusiveRegionWithSharedRegion
	self
		assertLock: (self regionLockFrom: 10 to: 20 exclusive: true)
		lock: (self regionLockFrom: 20 to: 30 exclusive: false)
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:26'!
testOverlappingSharedRegionWithExclusiveRegion
	self
		assertLock: (self regionLockFrom: 10 to: 20 exclusive: false)
		lock: (self regionLockFrom: 20 to: 30 exclusive: true)
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:26'!
testOverlappingSharedRegionWithSharedRegion
	self
		assertLock: (self regionLockFrom: 10 to: 20 exclusive: false)
		lock: (self regionLockFrom: 20 to: 30 exclusive: false)
		conflicts: false! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:21'!
testSupersetExclusiveRegionWithExclusiveRegion
	self
		assertLock: (self regionLockFrom: 10 to: 40 exclusive: true)
		lock: (self regionLockFrom: 20 to: 30 exclusive: true)
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:23'!
testSupersetExclusiveRegionWithSharedRegion
	self
		assertLock: (self regionLockFrom: 10 to: 40 exclusive: true)
		lock: (self regionLockFrom: 20 to: 30 exclusive: false)
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:23'!
testSupersetSharedRegionWithExclusiveRegion
	self
		assertLock: (self regionLockFrom: 10 to: 40 exclusive: false)
		lock: (self regionLockFrom: 20 to: 30 exclusive: true)
		conflicts: true! !

!UnixProcessFileLockTestCase methodsFor: 'tests' stamp: 'jf 2/22/2004 19:24'!
testSupersetSharedRegionWithSharedRegion
	self
		assertLock: (self regionLockFrom: 10 to: 40 exclusive: false)
		lock: (self regionLockFrom: 20 to: 30 exclusive: false)
		conflicts: false! !

!UnixProcessTestCase methodsFor: 'failures' stamp: 'eem 3/27/2014 11:29'!
expectedFailures
	^UnixOSProcessAccessor basicNew safeToForkSqueak
		ifTrue: [#()]
		ifFalse: [#(	testClassForkHeadlessSqueakAndDo
					testClassForkHeadlessSqueakAndDoThenQuit
					testClassForkSqueak
					testClassForkSqueakAndDo
					testClassForkSqueakAndDoThenQuit
					testEightLeafSqueakTree
					testForkHeadlessSqueakAndDo
					testForkHeadlessSqueakAndDoThenQuit
					testForkSqueak
					testForkSqueakAndDo
					testForkSqueakAndDoThenQuit
					testHeadlessChild
					testSpawnTenHeadlessChildren)]! !

!UnixProcessTestCase methodsFor: 'private' stamp: 'dtl 2/8/2019 23:03'!
numberOfOpenFiles
	"Answer the number of files currently open for this OS process. This works
	only on a system with a /proc filesystem and file descriptors located in a
	directory called /proc/<pid>/fd. On other systems, just answer 0."

	"UnixProcessTestCase new numberOfOpenFiles"

	| path |
	path _ '/proc/' , OSProcess thisOSProcess pid printString, '/fd'.
	(OSProcess directoryExists: path) ifFalse: [ ^ 0].
	^ OSProcess useFileMan
		ifTrue: [((Smalltalk at: #DirectoryEntry) perform: #withPathName: with: path) children size]
		ifFalse: [OSProcess useFileSystem
			ifTrue: [ (path perform: #asFileReference) children size ]
			ifFalse: [ ((Smalltalk at: #FileDirectory) on: path) entries size ]]
! !

!UnixProcessTestCase methodsFor: 'running' stamp: 'dtl 3/10/2002 10:52'!
runAll
	"If you get intermittent failures, try doing a garbage collect. Some of these
	tests can fail intermittently on the open file handle count checks"

	"UnixProcessTestCase new runAll"

	| result suite |
	Smalltalk garbageCollect.
	suite _ TestSuite new.
	suite addTest: (UnixProcessTestCase selector: #testClassForkSqueak).
	suite addTest: (UnixProcessTestCase selector: #testClassForkSqueakAndDo).
	suite addTest: (UnixProcessTestCase selector: #testClassForkSqueakAndDoThenQuit).
	suite addTest: (UnixProcessTestCase selector: #testClassForkHeadlessSqueakAndDo).
	suite addTest: (UnixProcessTestCase selector: #testClassForkHeadlessSqueakAndDoThenQuit).
	suite addTest: (UnixProcessTestCase selector: #testForkSqueak).
	suite addTest: (UnixProcessTestCase selector: #testForkSqueakAndDo).
	suite addTest: (UnixProcessTestCase selector: #testForkSqueakAndDoThenQuit).
	suite addTest: (UnixProcessTestCase selector: #testForkHeadlessSqueakAndDo).
	suite addTest: (UnixProcessTestCase selector: #testForkHeadlessSqueakAndDoThenQuit).
	suite addTest: (UnixProcessTestCase selector: #testHeadlessChild).
	suite addTest: (UnixProcessTestCase selector: #testSpawnTenHeadlessChildren).
	suite addTest: (UnixProcessTestCase selector: #testEightLeafSqueakTree).
	suite addTest: (UnixProcessTestCase selector: #testCatAFile).
	suite addTest: (UnixProcessTestCase selector: #testCatFromFileToFiles).
	suite addTest: (UnixProcessTestCase selector: #testRunCommand).
	suite addTest: (UnixProcessTestCase selector: #testPipe).
	suite addTest: (UnixProcessTestCase selector: #testPipeLine).

	result _ suite run.
	self should: [result defects size == 0].
	^ result
! !

!UnixProcessTestCase methodsFor: 'running' stamp: 'dtl 7/30/2010 16:47'!
setUp

	(self respondsTo: #timeout: ) "Recent Squeak images with test case timeout"
		ifTrue: [self perform: #timeout: with: 60].
	thisOSProcess _ OSProcess thisOSProcess
! !

!UnixProcessTestCase methodsFor: 'tests - class examples' stamp: 'dtl 6/11/2011 12:26'!
testCatAFile

	"(UnixProcessTestCase selector: #testCatAFile) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt: [p _ UnixProcess catAFile]
		raise: Warning.
	self assert: p notNil.
	self should: [p isRunning].
	[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
	self should: [p isComplete].
	self should: [p exitStatus == 0].
	self should: [p initialStdIn closed]. "Used a file for input, should be closed"
	self shouldnt: [p initialStdOut closed]. "Shared stdout with the VM, should be open"
	self shouldnt: [p initialStdErr closed]. "Shared stderr with the VM, should be open"
	self assert: self numberOfOpenFiles == openFileCount
! !

!UnixProcessTestCase methodsFor: 'tests - class examples' stamp: 'dtl 1/15/2018 22:14:01'!
testCatFromFileToFiles

	"(UnixProcessTestCase selector: #testCatFromFileToFiles) run"

	| p f openFileCount |
	openFileCount _ self numberOfOpenFiles.
	OSProcess deleteFileNamed: '/tmp/deleteMe.out'.
	OSProcess deleteFileNamed: '/tmp/deleteMe.err'.
	self shouldnt: [p _ UnixProcess catFromFileToFiles]
		raise: Warning.
	self assert: p notNil.
	self should: [p isRunning].
	[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
	self should: [p isComplete].
	self should: [p exitStatus == 0].
	self should: [p initialStdIn closed].
	self should: [p initialStdOut closed].
	self should: [p initialStdErr closed].
	f _ OSProcess oldFileNamed: '/tmp/deleteMe.out'.
	self shouldnt: [f upToEnd isEmpty].
	f close.
	f _ OSProcess oldFileNamed: '/tmp/deleteMe.err'.
	self should: [f upToEnd isEmpty].
	f close.
	self assert: self numberOfOpenFiles == openFileCount
! !

!UnixProcessTestCase methodsFor: 'tests - class examples' stamp: 'dtl 6/10/2011 15:55'!
testEightLeafSqueakTree

	"(UnixProcessTestCase selector: #testEightLeafSqueakTree) run"

	| a openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt: [a _ UnixProcess eightLeafSqueakTree]
		raise: Warning.
	self assert: (a isKindOf: Array).
	self assert: a size == 3.
	(a includes: 0)
		ifTrue:
			[Smalltalk quitPrimitive].
	self assert: self numberOfOpenFiles == openFileCount
! !

!UnixProcessTestCase methodsFor: 'tests - class examples' stamp: 'NM 10/16/2021 16:09:26'!
testExitCode 

	"The exit code you expect in POSIX systems. "

	|out|
	out _ OSProcess waitForCommand: ' sh -c "exit 33" '. 
	self assert: (out exitCode = 33). 


! !

!UnixProcessTestCase methodsFor: 'tests - class examples' stamp: 'dtl 6/29/2005 14:32'!
testHeadlessChild

	"(UnixProcessTestCase selector: #testHeadlessChild) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt: [p _ UnixProcess headlessChild]
		raise: Warning. "Catch warning if fork fails"
	[p isComplete] whileFalse: [(Delay forMilliseconds: 100) wait].
	self assert: p isComplete.
	self assert: p exitStatus == 0.
	self assert: self numberOfOpenFiles == openFileCount
! !

!UnixProcessTestCase methodsFor: 'tests - class examples' stamp: 'dtl 10/12/2001 08:32'!
testPipe

	"(UnixProcessTestCase selector: #testPipe) run"

	| openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self should: [UnixProcess testPipe = 'this is some text to write into the pipe'].
	self assert: self numberOfOpenFiles == openFileCount
! !

!UnixProcessTestCase methodsFor: 'tests - class examples' stamp: 'dtl 10/12/2001 08:32'!
testPipeLine

	"(UnixProcessTestCase selector: #testPipeLine) run"

	| openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self should: ['This is the text to write*' match: UnixProcess testPipeLine].
	self assert: self numberOfOpenFiles == openFileCount
! !

!UnixProcessTestCase methodsFor: 'tests - class examples' stamp: 'dtl 1/15/2018 22:14:16'!
testRunCommand

	"(UnixProcessTestCase selector: #testRunCommand) run"

	| p f openFileCount |
	openFileCount _ self numberOfOpenFiles.
	OSProcess deleteFileNamed: '/tmp/deleteMe.out'.
	OSProcess deleteFileNamed: '/tmp/deleteMe.err'.
	self shouldnt: [p _ UnixProcess testRunCommand]
		raise: Warning.
	self assert: p notNil.
	self should: [p isRunning].
	[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
	self should: [p isComplete].
	self should: [p exitStatus > 0].	"Forced an error exit status"
	"stdin was shared with the Squeak VM, so it should not have been closed."
	self shouldnt: [p initialStdIn closed].
	"but the output and error streams should have been closed."
	self should: [p initialStdOut closed].
	self should: [p initialStdErr closed].
	f _ OSProcess oldFileNamed: '/tmp/deleteMe.out'.
	self shouldnt: [f upToEnd isEmpty].
	f close.
	f _ OSProcess oldFileNamed: '/tmp/deleteMe.err'.
	self shouldnt: [f upToEnd isEmpty].
	f close.
	self assert: self numberOfOpenFiles == openFileCount


! !

!UnixProcessTestCase methodsFor: 'tests - class examples' stamp: 'dtl 6/29/2005 14:33'!
testSpawnTenHeadlessChildren

	"(UnixProcessTestCase selector: #testSpawnTenHeadlessChildren) run"

	| a openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt: [a _ UnixProcess spawnTenHeadlessChildren]
		raise: Warning. "Catch warning if fork fails"
	[(a detect: [:p | p isComplete not] ifNone: []) notNil]
		whileTrue: [(Delay forMilliseconds: 100) wait].
	self should: [(a select: [:p | p isComplete not]) isEmpty].
	self assert: self numberOfOpenFiles == openFileCount
! !

!UnixProcessTestCase methodsFor: 'tests - class side methods' stamp: 'dtl 6/29/2005 14:24'!
testClassForkHeadlessSqueakAndDo

	"(UnixProcessTestCase selector: #testClassForkHeadlessSqueakAndDo) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt:
		[p _ UnixProcess forkHeadlessSqueakAndDo:
			[(Delay forMilliseconds: 100) wait.
			OSProcess snapshot: false andQuit: true]]
		raise: Warning. "Catch warning if fork fails"
	self assert: p notNil.
	(p == thisOSProcess)
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - class side methods' stamp: 'dtl 6/29/2005 14:26'!
testClassForkHeadlessSqueakAndDoThenQuit

	"(UnixProcessTestCase selector: #testClassForkHeadlessSqueakAndDoThenQuit) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt:
		[p _ UnixProcess forkHeadlessSqueakAndDoThenQuit:
			[(Delay forMilliseconds: 100) wait]]
		raise: Warning. "Catch warning if fork fails"
	self assert: p notNil.
	(p == thisOSProcess)
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - class side methods' stamp: 'dtl 6/10/2011 15:38'!
testClassForkSqueak

	"(UnixProcessTestCase selector: #testClassForkSqueak) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt: [p _ UnixProcess forkSqueak]
		raise: Warning.
	self assert: p notNil.
	(p == thisOSProcess)
		ifTrue:
			["Child Squeak"
			p inspect.
			(Delay forMilliseconds: 100) wait.
			OSProcess snapshot: false andQuit: true]
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - class side methods' stamp: 'dtl 6/10/2011 15:43'!
testClassForkSqueakAndDo

	"(UnixProcessTestCase selector: #testClassForkSqueakAndDo) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt:
			[p _ UnixProcess forkSqueakAndDo:
					[(Delay forMilliseconds: 100) wait.
					OSProcess snapshot: false andQuit: true]]
		raise: Warning.
	self assert: p notNil.
	(p == thisOSProcess)
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - class side methods' stamp: 'dtl 6/10/2011 15:43'!
testClassForkSqueakAndDoThenQuit

	"(UnixProcessTestCase selector: #testClassForkSqueakAndDoThenQuit) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt:
			[p _ UnixProcess forkSqueakAndDoThenQuit:
					[(Delay forMilliseconds: 100) wait]]
		raise: Warning.
	self assert: p notNil.
	(p == thisOSProcess)
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - child process creation' stamp: 'dtl 6/10/2011 15:47'!
testForkHeadlessSqueakAndDo

	"(UnixProcessTestCase selector: #testForkHeadlessSqueakAndDo) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt:
			[p _ thisOSProcess forkHeadlessSqueakAndDo:
					[(Delay forMilliseconds: 100) wait.
					OSProcess snapshot: false andQuit: true]]
		raise: Warning. "Catch warning if fork fails"
	self assert: p notNil.
	(p == thisOSProcess)
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - child process creation' stamp: 'dtl 6/10/2011 15:47'!
testForkHeadlessSqueakAndDoThenQuit

	"(UnixProcessTestCase selector: #testForkHeadlessSqueakAndDoThenQuit) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt:
			[p _ thisOSProcess forkHeadlessSqueakAndDoThenQuit:
					[(Delay forMilliseconds: 100) wait]]
		raise: Warning. "Catch warning if fork fails"
	self assert: p notNil.
	(p == thisOSProcess)
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - child process creation' stamp: 'dtl 6/29/2005 14:30'!
testForkSqueak

	"(UnixProcessTestCase selector: #testForkSqueak) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt: [p _ thisOSProcess forkSqueak]
		raise: Warning. "Catch warning if fork fails"
	self assert: p notNil.
	(p == thisOSProcess)
		ifTrue:
			["Child Squeak"
			p inspect.
			(Delay forMilliseconds: 100) wait.
			OSProcess snapshot: false andQuit: true]
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - child process creation' stamp: 'dtl 6/10/2011 15:45'!
testForkSqueakAndDo

	"(UnixProcessTestCase selector: #testForkSqueakAndDo) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt:
			[p _ thisOSProcess forkSqueakAndDo:
					[(Delay forMilliseconds: 100) wait.
					OSProcess snapshot: false andQuit: true]]
		raise: Warning.
	self assert: p notNil.
	(p == thisOSProcess)
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - child process creation' stamp: 'dtl 6/10/2011 15:46'!
testForkSqueakAndDoThenQuit

	"(UnixProcessTestCase selector: #testForkSqueakAndDoThenQuit) run"

	| p openFileCount |
	openFileCount _ self numberOfOpenFiles.
	self shouldnt:
			[p _ thisOSProcess forkSqueakAndDoThenQuit:
					[(Delay forMilliseconds: 100) wait]]
		raise: Warning.
	self assert: p notNil.
	(p == thisOSProcess)
		ifFalse:
			["Parent Squeak process"
			self should: [p isRunning].
			[p isRunning] whileTrue: [(Delay forMilliseconds: 100) wait].
			self should: [p isComplete].
			self should: [p exitStatus == 0].
			self assert: self numberOfOpenFiles == openFileCount]
! !

!UnixProcessTestCase methodsFor: 'tests - child process creation' stamp: 'dtl 12/5/2020 11:33'!
testIsExternalProcess
	self deny: ThisOSProcess thisOSProcess isExternalProcess.
	self deny: OSProcess thisOSProcess isExternalProcess.
	self assert: ExternalOSProcess new isExternalProcess.
	self assert: (ExternalOSProcess command: 'pwd') isExternalProcess.! !

!UnixProcessTestCase class methodsFor: 'failures' stamp: 'eem 3/27/2014 11:20'!
expectedFailures
	^UnixOSProcessAccessor basicNew safeToForkSqueak
		ifTrue: [#()]
		ifFalse: [#(	testClassForkHeadlessSqueakAndDo
					testClassForkHeadlessSqueakAndDoThenQuit
					testClassForkSqueak
					testClassForkSqueakAndDo
					testClassForkSqueakAndDoThenQuit
					testForkHeadlessSqueakAndDo
					testForkHeadlessSqueakAndDoThenQuit
					testForkSqueak
					testForkSqueakAndDo
					testForkSqueakAndDoThenQuit
					testHeadlessChild
					testSpawnTenHeadlessChildren)]! !

!OSFileLock methodsFor: 'comparing' stamp: 'dtl 11/28/2010 09:18'!
= aFileLock
	^ ((self species = aFileLock species)
		and: [self fileStream == aFileLock fileStream])
			and: [self exclusive == aFileLock exclusive]! !

!OSFileLock methodsFor: 'comparing' stamp: 'jf 2/22/2004 18:32'!
hash
	^ (self fileStream hash + self exclusive hash) hashMultiply! !

!OSFileLock methodsFor: 'comparing' stamp: 'jf 2/22/2004 16:31'!
overlaps: aFileLock
	"Answer true if the receiver represents an addressable region that overlaps aFileLock"
	
	^ aFileLock fileStream fullName = self fileStream fullName! !

!OSFileLock methodsFor: 'comparing' stamp: 'jf 2/22/2004 16:35'!
overlapsRegion: aFileRegionLock
	^ aFileRegionLock fileStream fullName = self fileStream fullName! !

!OSFileLock methodsFor: 'conflict testing' stamp: 'jf 2/22/2004 16:38'!
conflictsWith: otherFileLock
	^ (self isExclusive or: [otherFileLock isExclusive])
		and: [self overlaps: otherFileLock]! !

!OSFileLock methodsFor: 'accessing' stamp: 'jf 2/22/2004 16:00'!
exclusive
	"Answer the value of exclusive. Default is true, indicating a read-write lock
	as opposed to a shared read lock."

	^ exclusive ifNil: [exclusive _ true].! !

!OSFileLock methodsFor: 'accessing' stamp: 'jf 2/22/2004 16:00'!
exclusive: anObject
	"Set the value of exclusive"

	exclusive _ anObject! !

!OSFileLock methodsFor: 'accessing' stamp: 'jf 2/22/2004 16:00'!
fileStream
	"Answer the value of fileStream"

	^ fileStream! !

!OSFileLock methodsFor: 'accessing' stamp: 'jf 2/22/2004 16:00'!
fileStream: anObject
	"Set the value of fileStream"

	fileStream _ anObject! !

!OSFileLock methodsFor: 'testing' stamp: 'jf 2/22/2004 18:13'!
isActive
	^ self fileStream closed not! !

!OSFileLock methodsFor: 'testing' stamp: 'jf 2/22/2004 16:01'!
isExclusive
	^ self exclusive! !

!OSFileLock methodsFor: 'private' stamp: 'jf 2/22/2004 18:07'!
length
	^ 0! !

!OSFileLock methodsFor: 'private' stamp: 'jf 2/22/2004 18:02'!
offset
	^ 0! !

!OSFileLock methodsFor: 'system locking' stamp: 'dtl 5/8/2006 06:52'!
lock
	"Answer true on success"

	^ self lockIfFail: [false]
! !

!OSFileLock methodsFor: 'system locking' stamp: 'dtl 8/19/2022 22:46'!
lockIfFail: failBlock

	| result |
	result _ ThisOSProcess accessor
		primLockFileRegion: fileStream fileID
		offset: self offset
		length: self length
		exclusive: self isExclusive.
	result == 0
		ifTrue: [^ true]
		ifFalse: [^ failBlock value]
! !

!OSFileLock methodsFor: 'system locking' stamp: 'dtl 8/19/2022 19:26'!
test
	"Answer true if this is a lockable file or region"

	| result |
	result _ ThisOSProcess accessor
		primTestLockableFileRegion: fileStream fileID
		offset: self offset
		length: self length
		exclusive: self isExclusive.
	^ (result == -1 or: [result isNil])
		ifTrue: [false]
		ifFalse: [result first]! !

!OSFileLock methodsFor: 'system locking' stamp: 'dtl 5/8/2006 06:58'!
unlock
	"Answer true on success"

	^ self unlockIfFail: [false]
! !

!OSFileLock methodsFor: 'system locking' stamp: 'dtl 8/19/2022 19:27'!
unlockIfFail: failBlock

	| result |
	result _ ThisOSProcess accessor
		primUnlockFileRegion: fileStream fileID
		offset: self offset
		length: self length.
	result == 0
		ifTrue: [^ true]
		ifFalse: [^ failBlock value]
! !

!OSFileLock methodsFor: 'initialize-release' stamp: 'jf 2/22/2004 16:02'!
onFile: aFileStream exclusive: writeLockFlag

	self fileStream: aFileStream.
	self exclusive: writeLockFlag
! !

!OSFileLock class methodsFor: 'instance creation' stamp: 'jf 2/22/2004 17:35'!
onFile: aFileStream exclusive: writeLockFlag
	"A region representing the whole file"

	^ super new onFile: aFileStream exclusive: writeLockFlag
! !

!OSFileRegionLock methodsFor: 'comparing' stamp: 'jf 2/22/2004 18:33'!
= aFileRegion

	^ (super = aFileRegion) and: [self interval = aFileRegion interval]
! !

!OSFileRegionLock methodsFor: 'comparing' stamp: 'jf 2/22/2004 18:33'!
hash

	^ (super hash + self interval hash) hashMultiply
! !

!OSFileRegionLock methodsFor: 'comparing' stamp: 'jf 2/22/2004 19:47'!
overlaps: aFileLock
	"Call #overlapsRegion: on aFileLock since we know we're a region but we don't know whether aFileLock is"
	
	^ aFileLock overlapsRegion: self! !

!OSFileRegionLock methodsFor: 'comparing' stamp: 'jf 2/22/2004 18:56'!
overlapsRegion: aFileRegionLock
	^ (super overlapsRegion: aFileRegionLock) and: [(self interval intersection: aFileRegionLock interval) isEmpty not]! !

!OSFileRegionLock methodsFor: 'accessing' stamp: 'dtl 3/10/2005 20:23'!
interval
	"Answer the value of interval"

	^ interval! !

!OSFileRegionLock methodsFor: 'accessing' stamp: 'dtl 3/10/2005 20:23'!
interval: anObject
	"Set the value of interval"

	interval _ anObject! !

!OSFileRegionLock methodsFor: 'private' stamp: 'jf 2/22/2004 18:02'!
length
	^ self interval size! !

!OSFileRegionLock methodsFor: 'private' stamp: 'jf 2/22/2004 18:02'!
offset
	^ self interval first! !

!OSFileRegionLock methodsFor: 'initialize-release' stamp: 'jf 2/22/2004 16:03'!
onFile: aFileStream interval: anInterval exclusive: writeLockFlag

	self onFile: aFileStream exclusive: writeLockFlag.
	self interval: anInterval.! !

!OSFileRegionLock class methodsFor: 'instance creation' stamp: 'dtl 2/22/2004 13:58'!
onFile: aFileStream from: start to: end exclusive: writeLockFlag
	"A region representing part of a file"

	^ self onFile: aFileStream interval: (start to: end) exclusive: writeLockFlag
! !

!OSFileRegionLock class methodsFor: 'instance creation' stamp: 'dtl 2/22/2004 13:59'!
onFile: aFileStream interval: anInterval exclusive: writeLockFlag
	"A region representing part of a file"

	^ super new onFile: aFileStream interval: anInterval exclusive: writeLockFlag
! !

!OSProcess methodsFor: 'printing' stamp: 'dtl 10/17/1999 21:12'!
printOn: aStream

	super printOn: aStream. aStream nextPutAll: ' with pid '. self pid printOn: aStream! !

!OSProcess methodsFor: 'initialize - release' stamp: 'dtl 5/31/1999 13:54'!
initialize

	self subclassResponsibility! !

!OSProcess methodsFor: 'testing' stamp: 'dtl 7/14/2001 21:12'!
isExternalProcess

	^ true! !

!OSProcess methodsFor: 'private' stamp: 'dtl 8/30/2003 10:11'!
noAccessorAvailable

	^ self class noAccessorAvailable
! !

!OSProcess methodsFor: 'accessing' stamp: 'dtl 3/18/2000 13:57'!
pid

	^ pid
! !

!OSProcess methodsFor: 'accessing' stamp: 'dtl 3/18/2000 13:58'!
pid: processIdentifier

	pid _ processIdentifier
! !

!OSProcess class methodsFor: 'external process access' stamp: 'dtl 6/27/2020 16:00'!
accessor
	"Answer an OSProcessAccessor for this OS process."

	^ self thisOSProcess ifNotNil: [ :osp | osp processAccessor ]
! !

!OSProcess class methodsFor: 'sUnit' stamp: 'dtl 1/15/2018 22:08:41'!
allTestResults
	"Run all available sUnit tests and save the results in a file named
	'OSProcessTestResults-<platform>.txt'. Display results on the Transcript as
	tests are run. Answer a string with the test results."

	"OSProcess allTestResults"

	| resultsFileName writeStream resultString results fs problems result wordSize |
	self listLoadedModules
		detect: [:e | '*OSProcessPlugin*' match: e]
		ifNone: [self notify: 'no OSProcessPlugin loaded'].
	wordSize _ (Smalltalk respondsTo: #wordSize)
		ifTrue: [Smalltalk wordSize]
		ifFalse: [4].
	resultsFileName _ 'OSProcessTestResults-' , OSProcess platformName , '-' ,
		(wordSize * 8) asString , 'bitImage-' ,
		(OSProcess accessor sizeOfPointer * 8) asString , 'bitHw.txt'.
	[fs _ self newFileNamed: resultsFileName.
	problems _ false.
	writeStream _ WriteStream on: String new.
	writeStream nextPutAll: self authorInitials , ' running OSProcess unit tests ' , DateAndTime now asString; cr.
	writeStream nextPutAll: 'OSProcess platformName => ' , OSProcess platformName; cr.
	writeStream nextPutAll: 'OSProcess platformSubtype => ' , OSProcess platformSubtype; cr.
	writeStream nextPutAll: 'OSProcess osVersion => ' , OSProcess osVersion; cr.
	writeStream nextPutAll: 'OSProcess vmVersion => ' , OSProcess vmVersion; cr.
	writeStream nextPutAll: 'size of C int: OSProcess accessor sizeOfInt ==> ' , OSProcess accessor sizeOfInt asString; cr.
	writeStream nextPutAll: 'size of C pointer: OSProcess accessor sizeOfPointer ==> ' , OSProcess accessor sizeOfPointer asString; cr.
	writeStream nextPutAll: 'OSProcess versionInformation asString => ' , OSProcess versionInformation asString; cr.
	Transcript show: writeStream contents.
	(((Class allSubInstances select: [:class | #'OSProcess-Tests' = class category]) ,
		(Class allSubInstances select: [:class | #'CommandShell-Tests' = class category]))
			reject: [:class | 'Abstract*' match: class name])
		do: [:testCase | 
			writeStream nextPutAll: 'running tests in ' , testCase name; cr.
			Transcript show: 'running tests in ' , testCase name; cr.
			testCase allTestSelectors
				do: [:testSelector |
					OSProcess thisOSProcess stdOut nextPutAll:
						testCase name, '>>',testSelector, String lf; flush.
					resultString _ (testSelector
								-> ([result _ (testCase selector: testSelector) run.
									result hasPassed
										ifFalse: [problems _ true].
									result]
										on: Error
										do: [:ex | problems _ true.
											'caught exception ' , ex printString])) printString.
					writeStream nextPutAll: resultString; cr.
					Transcript show: resultString; cr]].
	problems
		ifTrue: [writeStream nextPutAll: 'one or more problems found'; cr.
			Transcript show: 'one or more problems were found, see results file'; cr]
		ifFalse: [writeStream nextPutAll: 'all tests completed without problems'; cr.
			Transcript show: 'all tests completed without problems'; cr].
	results _ writeStream contents copyReplaceAll: String cr with: String lf.
	fs nextPutAll: results]
		ensure: [fs close].
	Transcript show: 'tests results have been saved in a file named ''' , resultsFileName , ''''; cr.
	^ results! !

!OSProcess class methodsFor: 'sUnit' stamp: 'dtl 1/20/2018 19:25:27'!
authorInitials
	"Get the author initials from the system."

	((Smalltalk respondsTo: #metacelloPlatformAttributes)
		and: [ 
			((Smalltalk perform: #metacelloPlatformAttributes) includes: #'pharo3.x')
				or: [ (Smalltalk perform: #metacelloPlatformAttributes) includes: #'pharo2.x' ] ])
		ifTrue: [ ^ (Smalltalk at: #Author) fullName select: [ :c | c isUppercase ] ].
	^ (Smalltalk at: #Utilities) authorInitials! !

!OSProcess class methodsFor: 'external system access' stamp: 'dtl 1/7/2001 12:32'!
arguments

	"OSProcess arguments"

	^ self thisOSProcess arguments
! !

!OSProcess class methodsFor: 'external system access' stamp: 'dtl 1/7/2001 12:32'!
programName

	"OSProcess programName"

	^ self thisOSProcess programName
! !

!OSProcess class methodsFor: 'private' stamp: 'dtl 1/7/2001 13:13'!
classForThisOSProcess
	"Answer the class which represents the OS process in which Squeak runs."

	^ ThisOSProcess concreteClass! !

!OSProcess class methodsFor: 'private' stamp: 'dtl 6/29/2005 14:18'!
noAccessorAvailable

	self notify: 'process accessor module not available'! !

!OSProcess class methodsFor: 'external command processing' stamp: 'dtl 1/7/2001 12:09'!
command: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in the standard C library,
	except that aCommandString runs asynchronously in a child process."

	"OSProcess command: 'ls -l /etc'"

	^ self thisOSProcess command: aCommandString
! !

!OSProcess class methodsFor: 'external command processing' stamp: 'dtl 5/3/2020 12:51'!
editOutputOf: aCommandString
	"Evaluate aCommandString and display the output in an editor"

	"OSProcess editOutputOf: 'ls -l' "
	"OSProcess editOutputOf: 'ps -axjf' "
 
	^ self outputOf: aCommandString, ' | edit'! !

!OSProcess class methodsFor: 'external command processing' stamp: 'dtl 2/8/2020 15:35'!
evaluate: aCommandString
	"Evaluate a command pipeline created from aCommandString. Answer a
	ProxyPipeline that evaluated the command string. Use #upToEndOfFile
	and #errorUpToEnd to read the standard output and standard error
	respectively. Open file handles will be closed after sending #upToEndOfFile,
	or may be closed explicitly with #closePipes for #finalize."

	"OSProcess evaluate: 'ls | cat | wc' "
	"OSProcess evaluate: 'ls -ldt * | edit' "
	"OSProcess evaluate: 'ls NOSUCHFILE * | cat | wc' "
	"OSProcess evaluate: 'ls | copyToOutput | wc' "
	"OSProcess evaluate: ''"

	Smalltalk at: #CommandShell
		ifPresent: [ :cls | ^ (Smalltalk at: #ProxyPipeline) command: aCommandString shell: cls new ].
	self notify: 'CommandShell not found'
! !

!OSProcess class methodsFor: 'external command processing' stamp: 'dtl 7/6/2022 10:56'!
outputOf: aCommandString
	"Evaluate a command pipeline created from aCommandString. Answer a
	the output of the evaluation. If the command string resulted in error outputs,
	then raise an error before answering the output result."

	"OSProcess outputOf: 'ls | cat | wc' "
	"OSProcess outputOf: 'ls * NOSUCHFILE' "
	"OSProcess outputOf: 'ls -ldt * | edit' "
	"OSProcess outputOf: 'ls | copyToOutput | wc' "

	|pipeline out err|
	pipeline _ self evaluate: aCommandString.
	out _ pipeline upToEndOfFile.
	err _ pipeline errorUpToEndOfFile.
	err isEmpty ifFalse: [self error: err].
	^ out
! !

!OSProcess class methodsFor: 'external command processing' stamp: 'dtl 1/7/2001 13:06'!
squeak
	"Start a new instance of Squeak running in a child OS process. The new instance
	will restart from the image file, so it is a clone of this image as it existed at the
	most recent image save. Note that subclasses can implement additional methods of
	starting Squeak images, especially for Unix systems."

	"OSProcess squeak"

	^ self thisOSProcess squeak

! !

!OSProcess class methodsFor: 'external command processing' stamp: 'dtl 1/7/2001 12:11'!
waitForCommand: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in the standard C library.
	The active Smalltalk process waits for completion of the external command process."

	"OSProcess waitForCommand: 'echo sleeping...; sleep 3; echo I just slept for three seconds'"

	^ self thisOSProcess waitForCommand: aCommandString
! !

!OSProcess class methodsFor: 'debugging' stamp: 'dtl 4/2/2005 12:32'!
debugMessage: aString
	"Print aString on standard output. The debug message is prefixed with the
	identity of the process in which the method is being evaluated, and the
	identity of the object which received the message. Useful for debugging
	timing or deadlock problems."

	[self thisOSProcess stdOut
		"The process in which the traced message is being evaluated"
		nextPutAll: Processor activeProcess hash printString, ':';
		"The identity of the object being traced"
		nextPutAll: thisContext sender sender sender receiver hash printString, ':';
		"The debug message"
		nextPutAll: aString asString; nextPut: Character lf; flush]
			on: Error
			do: []! !

!OSProcess class methodsFor: 'debugging' stamp: 'dtl 4/2/2005 12:32'!
trace
	"Print the sender's context on standard output. The debug message is
	prefixed with the identity of the process in which the method is being
	evaluated, and the identity of the object which received the message.
	Useful for debugging timing or deadlock problems."

	[self thisOSProcess stdOut
		"The process in which the traced message is being evaluated"
		nextPutAll: Processor activeProcess hash printString, ':';
		"The identity of the object being traced"
		nextPutAll: thisContext sender sender sender receiver hash printString, ':';
		"The method context describing the method being evaluated"
		nextPutAll: thisContext sender sender sender printString;
		nextPut: Character lf;
		flush]
			on: Error
			do: []! !

!OSProcess class methodsFor: 'debugging' stamp: 'dtl 4/2/2005 12:30'!
trace: debugMessageString
	"Print trace information followed by a debug message"

	[self thisOSProcess stdOut
		"The process in which the traced message is being evaluated"
		nextPutAll: Processor activeProcess hash printString, ':';
		"The identity of the object being traced"
		nextPutAll: thisContext sender sender sender receiver hash printString, ':';
		"The method context describing the method being evaluated"
		nextPutAll: thisContext sender sender sender printString, ':';
		nextPutAll: debugMessageString;
		nextPut: Character lf;
		flush]
			on: Error
			do: []! !

!OSProcess class methodsFor: 'version dependent' stamp: 'dtl 3/5/2005 11:32'!
getSystemAttribute: attributeID 
	"After Squeak version 3.6, #getSystemAttribute was moved to SmalltalkImage "

	^ ((Smalltalk classNamed: 'SmalltalkImage')
		ifNil: [^ Smalltalk getSystemAttribute: attributeID]) current getSystemAttribute: attributeID! !

!OSProcess class methodsFor: 'version dependent' stamp: 'dtl 1/20/2018 19:26:21'!
isPharo3AndLater
	"True if this image is a pharo of major version 3 or greater"

	Smalltalk
		at: #SystemVersion
		ifPresent: [:cls | ((cls canUnderstand: #type) and: [ cls canUnderstand: #major ])
			ifTrue: [^ (cls current perform: #type) = 'Pharo' and: [ (cls current perform: #major) >= 3 ]]].
	^false
! !

!OSProcess class methodsFor: 'version dependent' stamp: 'dtl 1/20/2018 19:27:17'!
isPharo5Update50558AndLater
	"True if this image is a pharo of major version 5 at upldate 50558 or greater"
	
	"OSProcess isPharo5Update50558AndLater"

	Smalltalk
		at: #SystemVersion
		ifPresent: [:cls | ((cls canUnderstand: #type) and: [ cls canUnderstand: #major ])
			ifTrue: [ ^ ((cls current perform: #type) = 'Pharo' and: [ (cls current perform: #major) >= 5 ])
						and: [ cls current  highestUpdate >= 50558]]].
	^false
! !

!OSProcess class methodsFor: 'version dependent' stamp: 'dtl 10/27/2007 10:55'!
listLoadedModules
	"After Squeak version 3.6, #listLoadedModules was moved to SmalltalkImage "

	^ ((Smalltalk classNamed: 'SmalltalkImage')
		ifNil: [^ Smalltalk listLoadedModules]) current listLoadedModules! !

!OSProcess class methodsFor: 'version dependent' stamp: 'ThierryGoubier 9/5/2013 11:13'!
osVersion
	"After Squeak version 3.6, #osVersion was moved to SmalltalkImage. Some
	versions of Pharo move this to OSPlatform and issue deprecation warnings
	about the other usages. Pharo3 deprecated OSPlatform direct access."

	"self osVersion"
	
	self isPharo3AndLater
		ifTrue: [ ^ (Smalltalk perform: #os) version ].
	^ (((Smalltalk hasClassNamed: #OSPlatform)
			and: [(Smalltalk at: #OSPlatform)
					respondsTo: #osVersion])
		ifTrue: [Smalltalk at: #OSPlatform]
		ifFalse: [((Smalltalk classNamed: 'SmalltalkImage')
				ifNil: [^ Smalltalk osVersion]) current]) osVersion! !

!OSProcess class methodsFor: 'version dependent' stamp: 'dtl 1/2/2014 20:54'!
platformName 
	"After Squeak version 3.6, #platformName was moved to SmalltalkImage Some
	versions of Pharo move this to OSPlatform and issue deprecation warnings
	about the other usages. The original idiom (Squeak 3.6 and earlier) is the sensible
	default, but here we attempt to support other implementations if the sensible
	default is not available."

	"OSProcess platformName"

	(Smalltalk respondsTo: #platformName)
		ifTrue: [^ Smalltalk platformName].
	self isPharo3AndLater
		ifTrue: [ ^ (Smalltalk perform: #os) name ].
	^ (((Smalltalk hasClassNamed: #OSPlatform)
			and: [(Smalltalk at: #OSPlatform)
					respondsTo: #platformName])
		ifTrue: [Smalltalk at: #OSPlatform]
		ifFalse: [((Smalltalk classNamed: 'SmalltalkImage')
				ifNil: [^ Smalltalk osVersion]) current]) platformName! !

!OSProcess class methodsFor: 'version dependent' stamp: 'dtl 9/4/2013 08:00'!
platformSubtype 
	"After Squeak version 3.6, #platformSubtype was moved to SmalltalkImage "

	"OSProcess platformSubtype"

	self isPharo3AndLater
		ifTrue: [ ^ (Smalltalk perform: #os) perform: #subtype ].
	^ ((Smalltalk classNamed: 'SmalltalkImage')
		ifNil: [^ Smalltalk platformSubtype]) current platformSubtype! !

!OSProcess class methodsFor: 'version dependent' stamp: 'dtl 1/20/2018 20:08:52'!
snapshot: save andQuit: quit
	"After Squeak version 3.6, #snapshot:andQuit: was moved to SmalltalkImage. Cuis has a
	different but distinct method signature so check for that first."
	
	"self snapshot: false andQuit: true"
	
	(Smalltalk respondsTo: #snapshot:andQuit:embedded:clearAllClassState:)
		ifTrue: [ "Cuis image"
			^ Smalltalk
				perform: #snapshot:andQuit:embedded:clearAllClassState:
				withArguments: { save . quit . false . false } ].
	^ ((Smalltalk classNamed: 'SmalltalkImage')
		ifNil: [^ Smalltalk
				perform: #snapshot:andQuit:embedded:
				withArguments: { save . quit . false } ])
			current snapshot: save andQuit: quit! !

!OSProcess class methodsFor: 'version dependent' stamp: 'dtl 1/15/2018 08:33:37'!
useFileMan
	"If true use FileMan for directory and file access. See senders for methods with file
	system dependencies."

	^ Smalltalk hasClassNamed: #FileIOAccessor! !

!OSProcess class methodsFor: 'version dependent' stamp: 'dtl 1/27/2013 21:48'!
useFileSystem
	"If true use FileSystem, otherwise use traditional FileDirectory. See senders
	for methods with file system dependencies."

	^ Smalltalk hasClassNamed: #FileReference! !

!OSProcess class methodsFor: 'version dependent' stamp: 'ThierryGoubier 9/5/2013 11:12'!
vmVersion 
	"After Squeak version 3.6, #vmVersion was moved to SmalltalkImage. Pharo3 deprecated vmVersion."

	self isPharo3AndLater
		ifTrue: [ ^ (Smalltalk perform: #vm) version ].
	^ ((Smalltalk classNamed: 'SmalltalkImage')
		ifNil: [^ Smalltalk vmVersion]) current vmVersion! !

!OSProcess class methodsFor: 'examples' stamp: 'dtl 7/12/2000 21:52'!
helloStdErr
	"Write a message on the standard error stream of the OS process, normally
	the terminal or window from which Squeak is being run. Most operating
	systems implement stdin, stdout, and stderr in some manner, so this shown
	as an OSProcess example even though the implemention is in my subclasses."

	"OSProcess helloStdErr"

	| this |
	this _ self thisOSProcess.
	this stdErr ifNil: [self noAccessorAvailable. ^ nil].
	^ this stdErr nextPutAll: 'Hello stderr'; nextPut: (Character lf); yourself! !

!OSProcess class methodsFor: 'examples' stamp: 'dtl 7/12/2000 21:53'!
helloWorld
	"Write a message on the standard output stream of the OS process, normally
	the terminal or window from which Squeak is being run. Most operating
	systems implement stdin, stdout, and stderr in some manner, so this shown
	as an OSProcess example even though the implemention is in my subclasses."

	"OSProcess helloWorld"

	| this |
	this _ self thisOSProcess.
	this stdOut ifNil: [self noAccessorAvailable. ^ nil].
	^ this stdOut nextPutAll: 'Hello world'; nextPut: Character lf; yourself! !

!OSProcess class methodsFor: 'examples' stamp: 'dtl 8/20/2022 00:39'!
readFromStdIn
	"Type some text on the standard input terminal, followed by <return> or <enter>,
	then call this method. Any available input text in the stdin stream will be read.
	This method sets standard input for the Squeak OS process for non-blocking reads
	in order to prevent the Smalltalk image from blocking on the read. After the read,
	standard input is set back to its normal blocking I/O mode.

	Most operating systems implement stdin, stdout, and stderr in some manner, so this
	is shown as an OSProcess example even though the implemention is in my subclasses."

	"OSProcess readFromStdIn inspect"

	| input ioHandle resultString |
	input _ self thisOSProcess stdIn.
	input ifNil: [self noAccessorAvailable. ^ nil].
	ioHandle _ input fileID.
	self accessor setNonBlocking: ioHandle.
	resultString _ self thisOSProcess stdIn next: 10000.
	self accessor setBlocking: ioHandle.
	^ resultString
! !

!OSProcess class methodsFor: 'initialize-release' stamp: 'dtl 10/19/2001 18:44'!
initialize

	"OSProcess initialize"

	UseIOHandle _ (Smalltalk hasClassNamed: #IOHandle)
! !

!OSProcess class methodsFor: 'platform identification' stamp: 'dtl 1/19/2021 21:39'!
isNonUnixMac
	"True if the platform is Mac OS prior to OSX"

	"OSProcess isNonUnixMac"

	| numericOsVersion |
	numericOsVersion _ [Integer readFrom: self osVersion readStream] on: Error do: [ 0 ].
	^ (self platformName = 'Mac OS') and: [numericOsVersion < 1000]
! !

!OSProcess class methodsFor: 'platform identification' stamp: 'dtl 3/5/2005 11:42'!
isOS2
	"True if the platform is OS2"
	"FIXME please - What is the correct platform name for OS2?"

	"OSProcess isOS2"

	^ self platformName = 'OS2'
! !

!OSProcess class methodsFor: 'platform identification' stamp: 'dtl 8/24/2003 09:52'!
isResponsibleForThisPlatform
	"Answer true if this class has responsibilities for the platform on which the
	Squeak VM is currently running."

	^ self subclassResponsibility! !

!OSProcess class methodsFor: 'platform identification' stamp: 'dtl 3/5/2005 11:43'!
isRiscOS
	"True if the platform is RiscOS"

	"OSProcess isRiscOS"

	^ self platformName = 'RiscOS'! !

!OSProcess class methodsFor: 'platform identification' stamp: 'dtl 1/19/2021 21:42'!
isUnix
	"True if the platform is Unix (including Linux, Mac OS X, or other unix-like OS).
	Note: Keep this method in sync with UnixOSProcessPlugin>>isResponsibleForThisPlatform."

	"OSProcess isUnix"

	| numericOsVersion |

	^ (self platformName = 'unix') or:
		[numericOsVersion _ [Integer readFrom: self osVersion readStream] on: Error do: [ 0 ].
		(self platformName = 'Mac OS') and: [numericOsVersion >= 1000]]
! !

!OSProcess class methodsFor: 'platform identification' stamp: 'dtl 1/19/2021 21:43'!
isUnixMac
	"True if the platform is Mac OS on OSX"

	"OSProcess isUnixMac"

	| osVersion numericOsVersion |
	osVersion _ self osVersion.
	^ ('darwin*' match: osVersion "Ian's VM")
		or: [numericOsVersion _ [Integer readFrom: osVersion readStream] on: Error do: [ 0 ].
			(self platformName = 'Mac OS') and: [numericOsVersion >= 1000] "John's VM"]
! !

!OSProcess class methodsFor: 'platform identification' stamp: 'dtl 3/5/2005 11:46'!
isWindows
	"True if the platform is an MS Windows OS"

	"OSProcess isWindows"

	^ self platformName = 'Win32'! !

!OSProcess class methodsFor: 'utility' stamp: 'dtl 1/24/2013 19:59'!
makeVM
	"Rebuild the virtual machine and plugins, assuming that this Squeak
	is running from a home directory in the appropriate place in the
	source code tree. If the build is successful, save the image and
	restart using the new VM."

	"OSProcess makeVM"

	^ self makeVmIn: OSProcess defaultPathString, OSProcess pathSeparator, 'build'
! !

!OSProcess class methodsFor: 'utility' stamp: 'dtl 1/25/2013 18:58'!
makeVmIn: buildDirectoryPathName
	"Rebuild the virtual machine and plugins in the buildDirectoryPathName
	directory. If the build is successful, save the image and restart using the
	new VM. This assumes that the currently executing VM is either located in,
	or linked to, the buildDirectoryPathName directory."

	"OSProcess makeVmIn: OSProcess defaultPathString, OSProcess pathSeparator, 'build'"

	^ self classForThisOSProcess makeVmIn: buildDirectoryPathName

! !

!OSProcess class methodsFor: 'utility' stamp: 'dtl 1/7/2001 12:35'!
quitAndRestart
	"Save image, start a new instance from the saved image, and quit this instance.
	This is useful if the VM has been recompiled or if a new pluggable primitive
	has been added."

	"OSProcess quitAndRestart"

	| firstPid this |
	firstPid _ OSProcess thisOSProcess pid.
	firstPid ifNil: 
			[self noAccessorAvailable.
			^ nil].
	Smalltalk saveSession.
	"Value of firstPid gets saved in the image"
	this _ OSProcess thisOSProcess.
	this pid = firstPid ifTrue: [self squeak ifNotNil: [Smalltalk quitPrimitive]].
	^ this! !

!OSProcess class methodsFor: 'utility' stamp: 'dtl 3/5/2005 11:34'!
systemAttributes
	"Answer a Dictionary of all of the system attributes which can be obtained from
	SystemDictionary>>getSystemAttribute."

	"OSProcess systemAttributes"

	| args idx a |
	args _ Dictionary new.

	idx _ -1.
	[a _ self getSystemAttribute: idx.
	a notNil and: [a size > 0]]
		whileTrue: [args at: idx put: a. idx _ idx - 1].

	a _ self getSystemAttribute: 0.
	(a notNil and: [a size > 0]) ifTrue: [args at: 0 put: a. idx _ idx - 1].
	a _ self getSystemAttribute: 1.
	(a notNil and: [a size > 0]) ifTrue: [args at: 1 put: a. idx _ idx - 1].
	a _ self getSystemAttribute: 2.
	(a notNil and: [a size > 0]) ifTrue: [args at: 2 put: a. idx _ idx - 1].

	idx _ 2.
	[a _ self getSystemAttribute: idx.
	a notNil and: [a size > 0]]
		whileTrue: [args at: idx put: a. idx _ idx + 1].

	idx _ 1001.
	[a _ self getSystemAttribute: idx.
	a notNil and: [a size > 0]]
		whileTrue: [args at: idx put: a. idx _ idx + 1].

	^ args.

! !

!OSProcess class methodsFor: 'instance creation' stamp: 'dtl 1/7/2001 12:03'!
thisOSProcess
	"Answer the single instance of the class corresponding to the OS process in which
	this Smalltalk image is executing."

	"OSProcess thisOSProcess"

	^ ThisOSProcess thisOSProcess
! !

!OSProcess class methodsFor: 'version testing' stamp: 'dtl 8/7/2003 07:28'!
versionInformation

	"OSProcess versionInformation"

	| osppVersion |
	osppVersion _ (Smalltalk hasClassNamed: #OSProcessPlugin)
		ifTrue:
			[(Smalltalk at: #OSProcessPlugin) versionInformation]
		ifFalse:
			['(not installed in this image)'].
	^ Array
		with: (self name, ' version ', self versionString)
		with: ((Smalltalk hasClassNamed: #CommandShell)
				ifTrue:
					[((Smalltalk at: #CommandShell) respondsTo: #versionString)
						ifTrue:
							['CommandShell version ', (Smalltalk at: #CommandShell) versionString]
						ifFalse:
							['CommandShell installed (old version, no versionString)']]
				ifFalse:
					['CommandShell is not installed'])
		with:  osppVersion
! !

!OSProcess class methodsFor: 'version testing' stamp: 'dtl 12/31/2023 12:03'!
versionString

	"OSProcess versionString"

	^ '4.7.6'! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 1/15/2018 20:51:19'!
defaultPathString

	^ self useFileMan
		ifTrue: [((Smalltalk at: #DirectoryEntry) perform: #currentDirectory) printString]
		ifFalse: [self useFileSystem
			ifTrue: [(((Smalltalk at: #Path) perform: #workingDirectory) perform: #asFileReference) pathString]
			ifFalse: [(Smalltalk at: #FileDirectory) default pathName]]
! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 2/8/2019 23:02'!
deleteFileNamed: fileName
	"Delete the file with the given name."
	
	self useFileMan
		ifTrue: [((Smalltalk at: #FileEntry) perform: #withPathName: with: fileName) delete]
		ifFalse: [self useFileSystem
			ifTrue: [ | file sel |
				file _ fileName perform: #asFileReference.
				(sel _ {#ensureDelete . #ensureDeleted . #delete}
					detect: [ :e | file respondsTo: e ]
					ifNone: [ self error: 'do not know how to ensureDelete' ]) notNil
						ifTrue: [ file perform: sel ].
				(file respondsTo: #ensureDelete)
					ifTrue: [ file perform: #ensureDelete ]
					ifFalse: [ file perform: #ensureDeleted ] ]
			ifFalse:
				[ (Smalltalk at: #FileDirectory) default deleteFileNamed: fileName ] ]! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 2/8/2019 23:02'!
directoryExists: path
	"Answer true if a directory of the given name exists. The given name may
	be either a full path name or a local directory within this directory."

	^ self useFileMan
		ifTrue: [((Smalltalk at: #DirectoryEntry) perform: #withPathName: with: path) exists]
		ifFalse: [self useFileSystem
			ifTrue: [ (path perform: #asFileReference) exists ]
			ifFalse: [ (Smalltalk at: #FileDirectory) default directoryExists: path ]]
! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 2/8/2019 23:02'!
fileExists: path
	"Answer true if a file of the given name exists. The given name may be
	either a full path name or a local file within this directory."

	^ self useFileMan
		ifTrue: [((Smalltalk at: #FileEntry) perform: #withPathName: with: path) exists]
		ifFalse: [self useFileSystem
			ifTrue: [ (path perform: #asFileReference) exists ]
			ifFalse: [ (Smalltalk at: #FileDirectory) default fileExists: path ]]
! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 2/8/2019 23:02'!
fileNamed: path
	^ self useFileMan
		ifTrue: [ | entry |
			entry _ ((Smalltalk at: #FileEntry) perform: #withPathName: with: path) assureExistence.
			entry fileAccessor privateWriteableFile: entry]
		ifFalse: [ FileStream fileNamed: path ]
! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 2/8/2019 23:02'!
newFileNamed: path
	^ self useFileMan
		ifTrue: [ ((Smalltalk at: #FileEntry) perform: #withPathName: with: path) writeStream ]
		ifFalse: [ FileStream fileNamed: path ]
! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 2/8/2019 23:02'!
oldFileNamed: path
	^ self useFileMan
		ifTrue: [ | entry |
			entry _ (Smalltalk at: #FileEntry) perform: #withPathName: with: path.
			entry fileAccessor privateWriteableFile: entry]
		ifFalse: [ FileStream fileNamed: path ]
! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 1/15/2018 09:04:06'!
pathSeparator

	^ self useFileMan
		ifTrue: [(Smalltalk at: #FileIOAccessor) default slash]
		ifFalse: [self useFileSystem
			ifTrue: [((Smalltalk at: #DiskStore) current perform: #delimiter) asString]
			ifFalse: [(Smalltalk at: #FileDirectory) slash]]
! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 2/8/2019 23:02'!
readOnlyFileNamed: path
	^ self useFileMan
		ifTrue: [ | entry |
			entry _ (Smalltalk at: #FileEntry) perform: #withPathName: with: path.
			entry fileAccessor privateReadOnlyFile: entry]
		ifFalse: [ FileStream readOnlyFileNamed: path ]
! !

!OSProcess class methodsFor: 'compatibility' stamp: 'dtl 3/21/2020 11:37'!
saveChangesInFileNamed: aString

	self useFileMan
		ifTrue: [ | oldChangesName newChangesName |
			(oldChangesName _ Smalltalk currentChangesName) ifNotNil: [
				Smalltalk closeSourceFiles. "so copying the changes file will always work"
				newChangesName _ self fullNameForChangesNamed: aString.
				(Smalltalk at: #FileIOAccessor) default
					copy: (oldChangesName perform: #asFileEntry) to: (newChangesName perform: #asFileEntry) ] ]
		ifFalse:
			[(Smalltalk respondsTo: #saveChangesInFileNamed:)
				ifTrue: [Smalltalk saveChangesInFileNamed: aString].
			(Smalltalk classNamed: #SmalltalkImage) current perform: #saveChangesInFileNamed: with: aString]! !

!OSProcess class methodsFor: 'utility - sysconf' stamp: 'dtl 10/18/2020 12:11'!
confnameTokens: tokenPrefix
	"Find the cpp macro definitions corresponding to tokenPrefix in system header files.
	Assumptions: Running on a gnu-linux platform with system configuration parameters
	defined in /usr/include/bits/confname.h"

	| include names |
	include _ '/usr/include/bits/confname.h'.
	names _ ( OSProcess outputOf: 'grep ', tokenPrefix, ' ', include, ' | grep #define' ) findTokens: String lf.
	^names collect: [:e |  (e findTokens: String tab, String space) second.].
! !

!OSProcess class methodsFor: 'utility - sysconf' stamp: 'dtl 10/18/2020 12:12'!
confstr
	"Answer a dictionary of available confstr(3) values for this platform.

	Assumptions: Running on a gnu-linux platform with system configuration parameters
	defined in /usr/include/bits/confname.h"

	"OSProcess sysconf"

	| tokens entries |
	tokens _ self confnameTokens: '_CS_'.
	entries _ tokens withIndexCollect: [ :e :i | e asSymbol -> ( OSProcess accessor confstr: i - 1 ) ].
	^Dictionary withAll: entries.! !

!OSProcess class methodsFor: 'utility - sysconf' stamp: 'dtl 10/18/2020 12:13'!
fpathconf: fileStream
	"For the given file stream, answer a dictionary of available fpathconf(3) values for this platform.

	Assumptions: Running on a gnu-linux platform with system configuration parameters
	defined in /usr/include/bits/confname.h"

	"OSProcess fpathconf: SourceFiles first"

	| aFileStream tokens entries |
	aFileStream _ SourceFiles first.
	tokens _ self confnameTokens: '_PC_'.
	entries _ tokens withIndexCollect: [ :e :i | e asSymbol -> ( OSProcess accessor fpathconf: aFileStream index: i - 1 ) ].
	^Dictionary withAll: entries.! !

!OSProcess class methodsFor: 'utility - sysconf' stamp: 'dtl 10/18/2020 12:35'!
gnuLinuxConf
	"Answer system configuration values for the current platform, assuming
	a gnu-linux or similar platform.. For file parameters use the Squeak sources
	file as a representative example."

	"OSProcess gnuLinuxConf"

	^ Dictionary withAll: {
		#sysconf -> self sysconf .
		#confstr -> self confstr .
		#fpathconf -> (self fpathconf: SourceFiles first)
	}.! !

!OSProcess class methodsFor: 'utility - sysconf' stamp: 'dtl 10/18/2020 12:14'!
pathconf: path
	"For a file at path, answer a dictionary of available pathconf(3) values for this platform.

	Assumptions: Running on a gnu-linux platform with system configuration parameters
	defined in /usr/include/bits/confname.h"

	"OSProcess pathconf: '/bin/date' "

	| aPath tokens entries |
	aPath _ '/bin/bash'.
	tokens _ self confnameTokens: '_PC_'.
	entries _ tokens withIndexCollect: [ :e :i | e asSymbol -> ( OSProcess accessor pathconf: aPath index: i - 1 ) ].
	^Dictionary withAll: entries.! !

!OSProcess class methodsFor: 'utility - sysconf' stamp: 'dtl 10/18/2020 12:14'!
sysconf
	"Answer a dictionary of available sysconf(3) values for this platform.

	Assumptions: Running on a gnu-linux platform with system configuration parameters
	defined in /usr/include/bits/confname.h"

	"OSProcess sysconf"

	| tokens entries |
	tokens _ self confnameTokens: '_SC_'.
	entries _ tokens withIndexCollect: [ :e :i | e asSymbol -> ( OSProcess accessor sysconf: i - 1 ) ].
	^Dictionary withAll: entries.! !

!ExternalOSProcess methodsFor: 'printing' stamp: 'dtl 3/18/2000 14:07'!
printOn: aStream

	super printOn: aStream.
	self isComplete ifTrue: [ aStream nextPutAll: ' (', self runState, ' with status ', self exitStatus printString, ')' ]
		ifFalse: [ aStream nextPutAll: ' (', self runState asString, ')' ]! !

!ExternalOSProcess methodsFor: 'accessing' stamp: 'dtl 11/8/2000 21:41'!
accessor
	"Answer an OSProcessAccessor which may be used to obtain information about
	the external OS process which I represent."

	^ OSProcess thisOSProcess processAccessor
! !

!ExternalOSProcess methodsFor: 'accessing' stamp: 'dtl 3/1/2002 06:38'!
initialStdErr
	"The stderr stream at the time the child process is invoked. If the same as
	stderr for the current Squeak process, it may change as a result of Squeak
	using its stderr stream. The child process may also modify its actual stderr;
	therefore this is not an accurate representation of the child process stderr
	during the life of the child process."

	^ initialStdErr! !

!ExternalOSProcess methodsFor: 'accessing' stamp: 'dtl 3/1/2002 06:38'!
initialStdErr: anExternalStream

	initialStdErr _ anExternalStream! !

!ExternalOSProcess methodsFor: 'accessing' stamp: 'dtl 3/1/2002 06:39'!
initialStdIn
	"The stdin stream at the time the child process is invoked. If the same as
	stdin for the current Squeak process, it may change as a result of Squeak
	using its stdin stream. The child process may also modify its actual stdin;
	therefore this is not an accurate representation of the child process stdin
	during the life of the child process."

	^ initialStdIn! !

!ExternalOSProcess methodsFor: 'accessing' stamp: 'dtl 3/1/2002 06:39'!
initialStdIn: anExternalStream

	initialStdIn _ anExternalStream! !

!ExternalOSProcess methodsFor: 'accessing' stamp: 'dtl 3/1/2002 06:39'!
initialStdOut
	"The stdout stream at the time the child process is invoked. If the same as
	stdout for the current Squeak process, it may change as a result of Squeak
	using its stdout stream. The child process may also modify its actual stdout;
	therefore this is not an accurate representation of the child process stdout
	during the life of the child process."

	^ initialStdOut! !

!ExternalOSProcess methodsFor: 'accessing' stamp: 'dtl 3/1/2002 06:40'!
initialStdOut: anExternalStream

	initialStdOut _ anExternalStream! !

!ExternalOSProcess methodsFor: 'accessing' stamp: 'dtl 1/25/2004 11:01'!
runState

	^ runState ifNil: [self unknownRunState]
! !

!ExternalOSProcess methodsFor: 'accessing' stamp: 'dtl 3/18/2000 12:30'!
runState: aSymbol

	runState _ aSymbol.
	self changed: #runState
! !

!ExternalOSProcess methodsFor: 'initialize - release' stamp: 'dtl 3/1/2002 06:41'!
closeInitialStdErr

	initialStdErr ifNotNil: [initialStdErr close]
! !

!ExternalOSProcess methodsFor: 'initialize - release' stamp: 'dtl 3/1/2002 06:42'!
closeInitialStdIn

	initialStdIn ifNotNil: [initialStdIn close]
! !

!ExternalOSProcess methodsFor: 'initialize - release' stamp: 'dtl 3/1/2002 06:42'!
closeInitialStdOut

	initialStdOut ifNotNil: [initialStdOut close]
! !

!ExternalOSProcess methodsFor: 'initialize - release' stamp: 'dtl 11/21/2006 14:04'!
closeStreams

	self closeInitialStdIn; closeInitialStdOut; closeInitialStdErr! !

!ExternalOSProcess methodsFor: 'initialize - release' stamp: 'dtl 10/6/2000 21:01'!
initialize

	self notYetRunning! !

!ExternalOSProcess methodsFor: 'setting run state' stamp: 'dtl 3/4/2001 18:55'!
complete
	"Process has exited and has been reaped. It no longer exists in the external operating system."

	self runState: #complete
! !

!ExternalOSProcess methodsFor: 'setting run state' stamp: 'dtl 3/26/2000 15:23'!
notYetRunning
	"Process has not yet entered running state."

	self runState: #notYetRunning
! !

!ExternalOSProcess methodsFor: 'setting run state' stamp: 'dtl 3/26/2000 15:23'!
running
	"Process is actively running."

	self runState: #running
! !

!ExternalOSProcess methodsFor: 'setting run state' stamp: 'dtl 10/6/2000 20:59'!
unknownRunState
	"Unable to determine the current run state of the process, possibly because
	this is a stale reference to a process which no longer exists."

	self runState: #unknownRunState
! !

!ExternalOSProcess methodsFor: 'testing' stamp: 'dtl 10/7/2000 14:45'!
isAccessible

	^ self accessor canAccessChildProcess: self! !

!ExternalOSProcess methodsFor: 'testing' stamp: 'dtl 1/20/2001 11:40'!
isComplete

	^ self runState == #complete! !

!ExternalOSProcess methodsFor: 'testing' stamp: 'dtl 1/20/2001 11:40'!
isNotYetRunning

	^ self runState == #notYetRunning! !

!ExternalOSProcess methodsFor: 'testing' stamp: 'dtl 1/20/2001 11:40'!
isRunning

	^ self runState == #running! !

!ExternalOSProcess methodsFor: 'testing' stamp: 'dtl 12/22/2001 18:32'!
succeeded
	"Answer true if my process completed successfully. Be optimistic here, and let
	my subclasses implement the details."

	^ self isComplete! !

!ExternalOSProcess methodsFor: 'updating' stamp: 'dtl 2/27/2002 09:45'!
update: aParameter
	"Notify any dependents if my run state changes. My subclasses will do additional
	updating when the run state changes."

	aParameter == #runState ifTrue: [self changed: #runState]
! !

!ExternalOSProcess class methodsFor: 'concrete subclasses' stamp: 'dtl 3/5/2005 12:02'!
concreteClass

	"ExternalOSProcess concreteClass"

	^ self subclasses
		detect: [:c | c isResponsibleForThisPlatform]
		ifNone: [self notify: self printString,
					': No concrete class implementation available for system type ',
					self platformName printString.
				nil]

! !

!ExternalOSProcess class methodsFor: 'instance creation' stamp: 'dtl 1/16/2001 05:36'!
exec: programName
	"Run a program in an external OS process, and answer an instance of myself
	which represents the external process."

	"ExternalOSProcess exec: '/bin/ls'"

	^ self concreteClass exec: programName
! !

!ExternalOSProcess class methodsFor: 'shell' stamp: 'SeanDeNigris 12/27/2013 12:30'!
shellFlags

	^ '-c'.! !

!ExternalMacOSProcess class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:45'!
isResponsibleForThisPlatform
	"Answer true if, for the current platform, this class is responsible for representing
	an OS process other than that in which the Squeak VM is currently running."

	^ self isNonUnixMac
! !

!ExternalOS2Process class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:47'!
isResponsibleForThisPlatform
	"Answer true if, for the current platform, this class is responsible for representing
	an OS process other than that in which the Squeak VM is currently running."

	^ self isOS2
! !

!ExternalRiscOSProcess class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:48'!
isResponsibleForThisPlatform
	"Answer true if, for the current platform, this class is responsible for representing
	an OS process other than that in which the Squeak VM is currently running."

	^ self isRiscOS
! !

!ExternalUnixOSProcess methodsFor: 'printing' stamp: 'dtl 8/23/2012 22:45'!
printOn: aStream

	self programName isNil
		ifTrue:
			[^ super printOn: aStream]
		ifFalse:
			[aStream
				nextPutAll: 'an ';
				nextPutAll: self class name, ' with pid ';
				nextPutAll: self pid printString;
				nextPutAll: ' on ';
				nextPutAll: programName;
				nextPutAll: ' (';
				nextPutAll: self runState asString.
			self isComplete
				ifTrue: [aStream nextPutAll: ', ';
					nextPutAll: (UnixProcessExitStatus for: exitStatus) printString].
			aStream nextPut: $)].
! !

!ExternalUnixOSProcess methodsFor: 'comparing' stamp: 'dtl 11/15/2013 10:31'!
= processProxy
	"True if this is a proxy that represents the same OS process as processProxy"

	^ ((processProxy isKindOf: ExternalUnixOSProcess)
			and: [pid = processProxy pid])
				and: [ppid = processProxy ppid]! !

!ExternalUnixOSProcess methodsFor: 'comparing' stamp: 'dtl 6/28/2010 22:26'!
hash

	^ (pid ifNil: [self identityHash]) + (ppid ifNil: [self identityHash])
! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 1/20/2001 12:48'!
arguments

	^ arguments! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 1/20/2001 12:49'!
arguments: arrayOfArgumentStrings

	arguments _ arrayOfArgumentStrings! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'NM 10/16/2021 15:59:09'!
exitCode
	"Return the exit code as expected in POSIX systems.  A number between 0 and 255.
	See notes in UnixProcessExitStatus. "
	^ (UnixProcessExitStatus for: exitStatus) exitStatus 
	
! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 7/3/1999 12:34'!
exitStatus

	^ exitStatus
! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 10/22/1999 22:21'!
exitStatus: anInteger

	exitStatus _ anInteger
! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 3/11/2001 09:17'!
initialEnvironment

	^ initialEnvironment! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 1/21/2001 11:31'!
initialEnvironment: aDictionary

	initialEnvironment _ aDictionary! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 7/3/1999 12:51'!
pid

	^ pid
! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 10/22/1999 22:21'!
pid: aPid

	pid _ aPid
! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 7/3/1999 12:33'!
ppid

	^ ppid
! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 10/22/1999 22:21'!
ppid: aPid

	ppid _ aPid
! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 1/20/2001 12:51'!
programName

	^ programName! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 1/20/2001 12:51'!
programName: fileName

	programName _ fileName! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 3/31/2001 17:08'!
pwd

	pwd ifNil: [pwd _ self accessor primGetCurrentWorkingDirectory].
	^ pwd! !

!ExternalUnixOSProcess methodsFor: 'accessing' stamp: 'dtl 3/31/2001 17:08'!
pwd: pathString

	pwd _ pathString! !

!ExternalUnixOSProcess methodsFor: 'initialize - release' stamp: 'dtl 4/4/2006 21:16'!
forkChild
	"Start the external OS process. All instances variables except for pid  
	should have been set. The pid will be set following creation of the new 
	external process. Creating a child process is the responsibility of the
	currently executing OS process, so request it to do so on behalf of this
	instance of ExternalUnixOSProcess."

	^ OSProcess thisOSProcess
		processProxy: self
		forkAndExec: programName
		arguments: arguments
		environment: initialEnvironment
		descriptors: (Array
				with: initialStdIn
				with: initialStdOut
				with: initialStdErr)
! !

!ExternalUnixOSProcess methodsFor: 'initialize - release' stamp: 'dtl 2/11/2001 19:07'!
initialize

	super initialize.
	^ self setDefaults
! !

!ExternalUnixOSProcess methodsFor: 'initialize - release' stamp: 'dtl 3/31/2001 17:10'!
setDefaults

	| this |
	this _ OSProcess thisOSProcess.
	initialEnvironment ifNil: [self initialEnvironment: this environment].
	initialStdIn ifNil: [self initialStdIn: this stdIn].
	initialStdOut ifNil: [self initialStdOut: this stdOut].
	initialStdErr ifNil: [self initialStdErr: this stdErr].	
	self pwd

! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:40'!
sigabrt
	"Send a SIGABRT signal to the external process which I represent."

	^ OSProcess thisOSProcess sigabrt: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:40'!
sigalrm
	"Send a SIGALRM signal to the external process which I represent."

	^ OSProcess thisOSProcess sigalrm: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:40'!
sigchld
	"Send a SIGCHLD signal to the external process which I represent."

	^ OSProcess thisOSProcess sigchld: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:40'!
sigcont
	"Send a SIGCONT signal to the external process which I represent."

	^ OSProcess thisOSProcess sigcont: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:40'!
sighup
	"Send a SIGHUP signal to the external process which I represent."

	^ OSProcess thisOSProcess sighup: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:40'!
sigint
	"Send a SIGINT signal to the external process which I represent."

	^ OSProcess thisOSProcess sigint: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:42'!
sigkill
	"Send a SIGKILL signal to the external process which I represent."

	^ OSProcess thisOSProcess sigkill: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:42'!
sigpipe
	"Send a SIGPIPE signal to the external process which I represent."

	^ OSProcess thisOSProcess sigpipe: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:42'!
sigquit
	"Send a SIGQUIT signal to the external process which I represent."

	^ OSProcess thisOSProcess sigquit: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:42'!
sigstop
	"Send a SIGSTOP signal to the external process which I represent."

	^ OSProcess thisOSProcess sigstop: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:43'!
sigterm
	"Send a SIGTERM signal to the external process which I represent."

	^ OSProcess thisOSProcess sigterm: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:43'!
sigusr1
	"Send a SIGUSR1 signal to the external process which I represent."

	^ OSProcess thisOSProcess sigusr1: self! !

!ExternalUnixOSProcess methodsFor: 'OS signal sending' stamp: 'dtl 12/5/2020 11:43'!
sigusr2
	"Send a SIGUSR2 signal to the external process which I represent."

	^ OSProcess thisOSProcess sigusr2: self! !

!ExternalUnixOSProcess methodsFor: 'testing' stamp: 'dtl 12/22/2001 18:30'!
succeeded

	^ self isComplete and: [self exitStatus == 0]! !

!ExternalUnixOSProcess methodsFor: 'terminating child' stamp: 'dtl 12/5/2020 12:51'!
terminate
	"Kill the child process that I represent. Set run state to unknown, expecting
	that the child process watcher will update it soon. If this is a stale reference
	to a child process that was created in an earlier session, then the child watcher
	will not see the exit, and run state will be left in the unknown state.
	Answer the result of the primitive, which will be -1 in the case of a child
	process that could not be signalled, possibly because this is a stale reference
	and the actual child no longer exists."

	| signalPrimResult |
	self isRunning ifTrue: [ self unknownRunState ].
	signalPrimResult _ self sigkill.
	self finalize.
	^ signalPrimResult


! !

!ExternalUnixOSProcess methodsFor: 'updating' stamp: 'dtl 7/6/2006 10:06'!
update: aParameter 

	| accessible |
	aParameter == #runState
		ifTrue: [| statusArray | 
			"Has the process exited?"
			statusArray _ self accessor primGetChildExitStatus: self pid.
			statusArray notNil
				ifTrue: [self exitStatus: (statusArray at: 2).
						^ self complete]].
	aParameter == #accessibility
		ifTrue: ["Does the process still exist, and is it reachable?"
			((accessible _ self isAccessible) notNil and: [accessible])
				ifFalse: [^ self isRunning ifTrue: [self unknownRunState]]].
	super update: aParameter
! !

!ExternalUnixOSProcess methodsFor: 'evaluating' stamp: 'dtl 11/21/2006 14:09'!
value
	"Start the external process"

	self isNotYetRunning ifTrue: [self forkChild; closeStreams]
! !

!ExternalUnixOSProcess class methodsFor: 'shells' stamp: 'dtl 1/23/2013 20:21'!
bashShellPath
	"A more full-featured shell from the Free Software Foundation"

	| path |
	path _ '/bin/bash'.
	(OSProcess fileExists: path)
		ifTrue: [^ path]
		ifFalse: [self notify: path, ' not found']! !

!ExternalUnixOSProcess class methodsFor: 'shells' stamp: 'dtl 1/23/2013 20:30'!
defaultShellPath
	"Default shell to run"

	| path |
	path _ '/bin/sh'.
	(OSProcess fileExists: path)
		ifTrue: [^ path]
		ifFalse: [self notify: path, ' not found']! !

!ExternalUnixOSProcess class methodsFor: 'shells' stamp: 'dtl 1/23/2013 20:21'!
remoteShellPath
	"A remote shell processor. This may need to be edited for different systems."

	| path |
	path _ '/usr/bin/rsh'.
	(OSProcess fileExists: path)
		ifTrue: [^ path]
		ifFalse: [self notify: path, ' not found']! !

!ExternalUnixOSProcess class methodsFor: 'shells' stamp: 'dtl 1/23/2013 20:21'!
tkShellPath
	"The wish command shell for Tk/Tcl. This tends to be installed in a wide variety
	of places, so you may need to edit this method for your system."

	| path |
	path _ '/usr/bin/wish'.
	(OSProcess fileExists: path)
		ifTrue: [^ path]
		ifFalse: [self notify: path, ' not found']! !

!ExternalUnixOSProcess class methodsFor: 'instance creation' stamp: 'dtl 7/12/2003 11:38'!
command: aCommandString

	"ExternalUnixOSProcess command: 'ls -l /etc'"

	^ self forkAndExec: self defaultShellPath
		arguments: (Array with: '-c' with: aCommandString)
		environment: nil! !

!ExternalUnixOSProcess class methodsFor: 'instance creation' stamp: 'dtl 1/16/2001 05:33'!
exec: programName
	"Run a program in an external OS process, and answer an instance of myself
	which represents the external process."

	^ self forkAndExec: programName
! !

!ExternalUnixOSProcess class methodsFor: 'instance creation' stamp: 'dtl 2/27/2002 15:25'!
forkAndExec: executableFile

	"ExternalUnixOSProcess forkAndExec: '/bin/ls'"

	^ super new
		programName: executableFile;
		initialize;
		forkChild
! !

!ExternalUnixOSProcess class methodsFor: 'instance creation' stamp: 'dtl 2/27/2002 15:25'!
forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary 
	"Run a program in an external OS process, and answer an instance of 
	myself which represents the external process."

	"ExternalUnixOSProcess forkAndExec: '/bin/ls' arguments: (Array with: '-l') environment: (UnixProcess env)"

	^ super new
		programName: executableFile;
		arguments: arrayOfStrings;
		initialEnvironment: stringDictionary;
		initialize;
		forkChild
! !

!ExternalUnixOSProcess class methodsFor: 'instance creation' stamp: 'dtl 2/27/2002 15:25'!
forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams
	"Run a program in an external OS process, and answer an instance of myself
	which represents the external process."

	"ExternalUnixOSProcess
		forkAndExec: '/bin/ls'
		arguments: (Array with: '-l')
		environment: (UnixProcess env)
		descriptors: nil"

	| proc |
	proc _ super new
		programName: executableFile;
		arguments: arrayOfStrings;
		initialEnvironment: stringDictionary.
	arrayOf3Streams ifNotNil:
		[proc initialStdIn: (arrayOf3Streams at: 1).
		proc initialStdOut: (arrayOf3Streams at: 2).
		proc initialStdErr: (arrayOf3Streams at: 3)].
	^ proc initialize forkChild
! !

!ExternalUnixOSProcess class methodsFor: 'instance creation' stamp: 'dtl 2/27/2002 15:25'!
forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams workingDir: pathString
	"Run a program in an external OS process, and answer an instance of myself
	which represents the external process."

	"ExternalUnixOSProcess forkAndExec: '/bin/ls' arguments: nil environment: nil descriptors: nil workingDir: '/etc'"

	| proc |
	proc _ super new
		programName: executableFile;
		arguments: arrayOfStrings;
		initialEnvironment: stringDictionary.
	arrayOf3Streams ifNotNil:
		[proc initialStdIn: (arrayOf3Streams at: 1).
		proc initialStdOut: (arrayOf3Streams at: 2).
		proc initialStdErr: (arrayOf3Streams at: 3)].
	pathString ifNotNil: [proc pwd: pathString].
	^ proc initialize forkChild
! !

!ExternalUnixOSProcess class methodsFor: 'instance creation' stamp: 'dtl 2/28/2002 10:15'!
programName: executableFile arguments: arrayOfStrings initialEnvironment: stringDictionary
	"Answer an instance not yet running."

	^ super new
		programName: executableFile;
		arguments: arrayOfStrings;
		initialEnvironment: stringDictionary
! !

!ExternalUnixOSProcess class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:48'!
isResponsibleForThisPlatform
	"Answer true if, for the current platform, this class is responsible for representing
	an OS process other than that in which the Squeak VM is currently running."

	^ self isUnix
! !

!ExternalWindowsOSProcess methodsFor: 'printing' stamp: 'dtl 2/26/2002 10:42'!
printOn: aStream

	self commandLine isNil
		ifTrue:
			[^ super printOn: aStream]
		ifFalse:
			[aStream
				nextPutAll: 'a ';
				nextPutAll: self class name, ' with pid ';
				nextPutAll: self pid printString.
			(self isComplete and: [handle isNil])
				ifTrue:
					[aStream nextPutAll: ' (handle closed)']
				ifFalse:
					[aStream nextPutAll: ' handle ';
						nextPutAll: (handle isNil
							ifTrue: [handle printString]
							ifFalse: [handle asArray printString])].
			aStream
				nextPutAll: ' on ''';
				nextPutAll: commandLine;
				nextPutAll: ''' (';
				nextPutAll: self runState asString;
				nextPut: $)]

! !

!ExternalWindowsOSProcess methodsFor: 'comparing' stamp: 'dtl 11/15/2013 10:30'!
= processProxy
	"True if this is a proxy that represents the same OS process as processProxy"

	^ ((processProxy isKindOf: ExternalWindowsOSProcess)
			and: [pid = processProxy pid])
				and: [ppid = processProxy ppid]! !

!ExternalWindowsOSProcess methodsFor: 'comparing' stamp: 'dtl 6/28/2010 22:27'!
hash

	^ (pid ifNil: [self identityHash]) + (ppid ifNil: [self identityHash])
! !

!ExternalWindowsOSProcess methodsFor: 'initialize - release' stamp: 'dtl 3/1/2002 07:44'!
closeHandles
	"Clean up after process exits."

	self threads do: [:thread | thread closeHandle].
	handle ifNotNil:
		[OSProcess accessor primCloseHandle: handle.
		handle _ nil]
! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/26/2002 10:09'!
commandLine

	^ commandLine! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/26/2002 10:09'!
commandLine: aCommandLineString

	commandLine _ aCommandLineString! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/25/2002 08:32'!
exitStatus

	^ exitStatus! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/25/2002 08:33'!
exitStatus: status

	exitStatus _ status
! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/25/2002 07:24'!
handle
	"A Windows HANDLE for this OS process, represented as a ByteArray. The
	handle should be closed when the process exits."

	^ handle! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/25/2002 07:32'!
handle: aHandleObject
	"A Windows HANDLE for this OS process, represented as a ByteArray. The
	handle should be closed when the process exits."

	handle _ aHandleObject! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/25/2002 07:29'!
ppid

	^ ppid
! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/25/2002 07:29'!
ppid: aProcessID

	ppid _ aProcessID! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/28/2002 10:28'!
pwd
	"Answer the current working directory string."

	^ pwd! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/28/2002 10:28'!
pwd: pathString
	"The current working directory string."

	pwd _ pathString! !

!ExternalWindowsOSProcess methodsFor: 'accessing' stamp: 'dtl 2/25/2002 07:09'!
threads
	"One or more threads of execution within the OS process"

	^ threads ifNil: [threads _ OrderedCollection new]
! !

!ExternalWindowsOSProcess methodsFor: 'setting run state' stamp: 'dtl 6/24/2006 08:41'!
complete
	"Process has exited and has been reaped. It no longer exists in the external operating system."

	(threads notNil and: [threads size > 0]) ifTrue: [threads do: [:t | t complete]].
	self closeHandles.
	super complete
! !

!ExternalWindowsOSProcess methodsFor: 'updating' stamp: 'dtl 2/27/2002 09:45'!
update: aParameter 

	| accessible stat |
	aParameter == #runState
		ifTrue:
			["Has the process exited?"
	 		stat _ self accessor primGetExitStatusForHandle: self handle.
			stat ifNotNil: [self complete closeHandles exitStatus: stat]].
	aParameter == #accessibility
		ifTrue: ["Does the process still exist, and is it reachable?"
			((accessible _ self isAccessible) notNil and: [accessible])
				ifFalse: [self isRunning ifTrue: [self unknownRunState]]].
	super update: aParameter
! !

!ExternalWindowsOSProcess methodsFor: 'evaluating' stamp: 'ThierryGoubier 7/27/2017 22:38'!
value
	"Start the external process"

	| procInfo mainThread |
	self isNotYetRunning ifTrue:
		[procInfo _ OSProcess accessor primCommand: self commandLine.
		procInfo isNil
			ifTrue:
				[ | err | (err _ self initialStdErr) notNil ifTrue: [
					err nextPutAll: 'cannot execute ', self commandLine; cr ].
				self exitStatus: #cannotExecuteCommandLine.
				"FIXME: Close the OSPipes now, otherwise the image will block on a read"
				self closeStreams.
				[self complete] fork "defer execution so OSPipes stay in place for now"]
			ifFalse:
				[self pid: (procInfo at: 3).
				self handle: (procInfo at: 1).
				mainThread _ WindowsThread
						threadID: (procInfo at: 4)
						handle: (procInfo at: 2)
						running: true.
				self threads add: mainThread.
 				self running.
				OSProcess thisOSProcess registerChildProcess: self.
				"FIXME: Close the initial pipe handles. For now, I have not implemented
				passing these to the child, and there is no support yet for nonblocking
				Windows OS pipes. Once those are available, this method needs to change
				to support."
				self closeStreams]].
! !

!ExternalWindowsOSProcess class methodsFor: 'instance creation' stamp: 'dtl 2/28/2002 10:56'!
command: aCommandString

	"ExternalWindowsOSProcess command: 'SOL'"
	"ExternalWindowsOSProcess command: 'NoSuchProgram'"

	^ (self commandNoEvaluate: aCommandString) value
! !

!ExternalWindowsOSProcess class methodsFor: 'instance creation' stamp: 'dtl 2/28/2002 10:49'!
commandNoEvaluate: aCommandString
	"Answer an instance not yet running."

	"ExternalWindowsOSProcess commandNoEvaluate: 'SOL'"

	^ super new
		commandLine: aCommandString;
		ppid: OSProcess thisOSProcess pid;
		notYetRunning
! !

!ExternalWindowsOSProcess class methodsFor: 'instance creation' stamp: 'dtl 3/1/2002 07:02'!
programName: executableFile arguments: arrayOfStrings initialEnvironment: stringDictionary
	"This is for protocol compatibility with ExternalUnixOSProcess. For now, just reassemble
	a command line string and ignore the environment argument."

	| commandLine |
	commandLine _ WriteStream on: String new.
	commandLine nextPutAll: executableFile.
	arrayOfStrings ifNotNil:
		[arrayOfStrings do: [:arg | commandLine nextPut: Character space; nextPutAll: arg]].
	^ self commandNoEvaluate: commandLine contents
! !

!ExternalWindowsOSProcess class methodsFor: 'shells' stamp: 'SeanDeNigris 12/27/2013 10:17'!
defaultShellPath

	^ 'C:\Windows\System32\cmd.exe'.! !

!ExternalWindowsOSProcess class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:50'!
isResponsibleForThisPlatform
	"Answer true if, for the current platform, this class is responsible for representing
	an OS process other than that in which the Squeak VM is currently running."

	^ self isWindows
! !

!ExternalWindowsOSProcess class methodsFor: 'shell' stamp: 'SeanDeNigris 12/27/2013 12:31'!
shellFlags

	^ '/c'.! !

!ThisOSProcess methodsFor: 'comparing' stamp: 'dtl 11/15/2013 10:30'!
= processProxy
	"True if this is a proxy that represents the same OS process as processProxy"

	^ ((processProxy isKindOf: ThisOSProcess)
			and: [pid = processProxy pid])
		and: [sessionID = processProxy sessionID]! !

!ThisOSProcess methodsFor: 'comparing' stamp: 'dtl 6/27/2010 22:38'!
hash

	^ (pid ifNil: [self identityHash])
		+ (sessionID
				ifNil: [0]
				ifNotNil: [sessionID sum])! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 6/5/2015 20:13'!
activeChildren
	"Answer child processes which are currently believed to be running."

	"OSProcess thisOSProcess activeChildren"

	^ accessProtect critical: [ childProcessList select: [ :p | p isComplete not ] ].
! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 2/6/2015 19:50'!
allMyChildren

	^ Dictionary withAll: (childProcessList collect: [ :e | e pid -> e ])! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 2/6/2015 20:11'!
childPids

	^ childProcessList collect: [ :p | p pid ]
! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 1/20/2018 19:59:40'!
discardExitedChildren
	"Remove entries for completed child processed from dictionary.
	nb Cuis does not have #select:thenDo:"

	self updateAllMyChildren.
	(childProcessList select: [ :p | p isComplete ])
		do: [ :e | self unregisterChildProcess: e ].
	^ childProcessList
! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 2/6/2015 20:14'!
exitedChildren
	"Answer child processes which have exited and are no longer running."

	"OSProcess thisOSProcess exitedChildren"

	^ accessProtect critical: [ childProcessList select: [ :p | p isComplete ]].
! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 2/6/2015 20:03'!
pruneExitedChildrenAfter: size
	"Limit the size of the child process registry. Select the last entries, and unregister
	them if they are no longer active."

	"OSProcess thisOSProcess pruneExitedChildrenAfter: 5"

	(accessProtect critical: [childProcessList allButFirst: size])
		do: [ :e |
			e isComplete ifTrue: [ self unregisterChildProcess: e ]]
! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 2/6/2015 20:54'!
registerChildProcess: anOSProcess
	"Unregister anOSProcess, and trim the child process registry to prevent excessive
	accumulation of exited children."

	accessProtect critical: [ childProcessList addFirst: anOSProcess ].
	self pruneExitedChildrenAfter: ChildListSize.
	^ anOSProcess
! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 2/6/2015 19:49'!
unregisterChildProcess: anOSProcess

	accessProtect
		critical: [childProcessList remove: anOSProcess ifAbsent: [] ].
	^ anOSProcess
! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 2/25/2002 23:23'!
updateActiveChildren
	"Test each active child for its completion status and update runState and exitStatus
	accordingly. This method may be called when a semaphore is set indicating that
	some child OSProcess has died. A better approach might be to use an event queue
	for death of child events; however, until event queues are part of Squeak image,
	this polling mechanism is sufficient."

	self activeChildren do:
		[:child | child update: #runState]
! !

!ThisOSProcess methodsFor: 'child process management' stamp: 'dtl 2/4/2015 23:38'!
updateAllMyChildren
	"Test each child to make sure that it is still accessible. If the child is believed to be
	running, check to see if it has exited, and update runState and exitStatus accordingly."

	| children |
	accessProtect critical: [children _ self allMyChildren asArray].
	children do:
		[:child |
		child update: #accessibility.
		child isRunning ifTrue: [child update: #runState]]
! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/5/2003 21:59'!
canConnectToXDisplay: xDisplayName
	"Open and close a connection to displayName. It the connection was successfully
	opened, answer true; otherwise false. This is intended to check for the ability
	to open an X display prior to actually making the attempt."

	"self thisOSProcess canConnectToXDisplay: ':0.0' "
	"self thisOSProcess canConnectToXDisplay: ':1' "
	"self thisOSProcess canConnectToXDisplay: 'bogus:0' " "<-make sure network is running first!!"

	(xDisplayName isKindOf: String) ifFalse: [^ false].
	^ self processAccessor primCanConnectToXDisplay: xDisplayName
! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/6/2003 06:35'!
closeXDisplay
	"Become headless by closing the X session. All subsequent processing should involve
	no further display interaction."

	"self thisOSProcess closeXDisplay"

	| proc |
	proc _ self processAccessor primKillDisplay.
	proc ifNil: [self noAccessorAvailable].
	^ proc
! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/5/2003 22:05'!
currentXDisplayName

	"self thisOSProcess currentXDisplayName"

	^ self processAccessor primGetXDisplayName! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/6/2003 06:35'!
decapitate
	"Become headless by closing the X session. All subsequent processing should involve
	no further display interaction."

	"self thisOSProcess decapitate"

	^ self closeXDisplay
! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/6/2003 06:30'!
disconnectXDisplay
	"Disconnect from the X server, but do not close the existing Squeak window. A new
	display medium must be opened before further interaction with the display."

	"self thisOSProcess disconnectXDisplay"

	| proc |
	proc _ self processAccessor primDisconnectDisplay.
	proc ifNil: [self noAccessorAvailable].
	^ proc
! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/5/2003 22:12'!
displayOnXServer: xDisplayName
	"Check if it is possible to open a display on the X server identified by
	xDisplayName. If so, close the current X display and reopen it on
	the new server. On success, answer the previous display name. On
	failure, answer a string with an error message. This method is expected
	to be called by a web server or other application which may wish to
	make use of the result string."

	"self thisOSProcess displayOnXServer: ':0.0' "
	"self thisOSProcess displayOnXServer: 'unix:0' "
	"self thisOSProcess displayOnXServer: ':1' "
	"self thisOSProcess displayOnXServer: 'noSuchMachine'"
	"self thisOSProcess displayOnXServer: 'noSuchMachine:0'" "<-make sure network is running first!!"

	| previousDisplayName |
	(xDisplayName isKindOf: String)
		ifFalse: [^ 'expected display name string'].
	previousDisplayName _ self currentXDisplayName.
	(self canConnectToXDisplay: xDisplayName)
		ifTrue:
			[self decapitate.
			self setXDisplayName: xDisplayName.
			self recapitate.
			^ previousDisplayName]
		ifFalse:
			[^ 'cannot connect to display ', xDisplayName]
! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 9/1/2003 13:15'!
flushXDisplay
	"Synchronize output to the X display."

	"self thisOSProcess flushXDisplay"

	^ self processAccessor primFlushXDisplay

! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/24/2003 10:12'!
isConnectedToXServer
	"Answer true if VM is currently connected to an X server."

	"self thisOSProcess isConnectedToXServer"

	^ self processAccessor primIsConnectedToXServer

! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/6/2003 06:36'!
openXDisplay
	"Restore headful display opening the X session."

	"self thisOSProcess closeXDisplay. (Delay forSeconds: 5) wait. self thisOSProcess openXDisplay."

	| proc |
	proc _ self processAccessor primOpenXDisplay.
	proc ifNil: [self noAccessorAvailable].
	^ proc
! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/6/2003 06:34'!
recapitate
	"Restore headful display opening the X session."

	"self thisOSProcess decapitate. (Delay forSeconds: 5) wait. self thisOSProcess recapitate."

	^ self openXDisplay
! !

!ThisOSProcess methodsFor: 'display management' stamp: 'dtl 8/5/2003 22:16'!
setXDisplayName: xDisplayName
	"Set X display name for use by the next call to recapitate"

	"self thisOSProcess setXDisplayName: 'unix:0' "
	"self thisOSProcess setXDisplayName: ':1' "

	self processAccessor primSetXDisplayName: xDisplayName

! !

!ThisOSProcess methodsFor: 'child process creation' stamp: 'dtl 10/18/2001 20:15'!
command: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in
	the standard C library, except that aCommandString runs asynchronously
	in a child process. Answer an instance of ExternalMacOSProcess which
	is a proxy for the new OS process."

	self subclassResponsibility! !

!ThisOSProcess methodsFor: 'child process creation' stamp: 'dtl 10/18/2001 20:16'!
forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams
	"Use my processAccessor to call vfork() and execve() and create a
	new child task. Answer a proxy for the new task, an instance of
	ExternalOSProcess."

	self subclassResponsibility! !

!ThisOSProcess methodsFor: 'finalization' stamp: 'dtl 11/4/2000 15:49'!
finalize

	processAccessor
		ifNotNil: 
			[processAccessor removeDependent: self.
			processAccessor finalize].
	processAccessor _ nil! !

!ThisOSProcess methodsFor: 'environment' stamp: 'dtl 10/18/2001 20:10'!
getCwd

	self subclassResponsibility! !

!ThisOSProcess methodsFor: 'private - IOHandle' stamp: 'dtl 1/20/2018 19:59:54'!
handleFromAccessor: aByteArrayOrIOAccessor

	UseIOHandle
		ifTrue: [aByteArrayOrIOAccessor isNil
			ifTrue: [^ nil]
			ifFalse: [^ aByteArrayOrIOAccessor perform: #asSQFileStruct]]
		ifFalse: [^ aByteArrayOrIOAccessor]
! !

!ThisOSProcess methodsFor: 'private - IOHandle' stamp: 'dtl 1/20/2018 19:57:15'!
handleFromFileStream: aFileStream

	UseIOHandle
		ifTrue: [^ aFileStream ioHandle perform: #asSQFileStruct]
		ifFalse: [^ aFileStream fileID]
! !

!ThisOSProcess methodsFor: 'private - IOHandle' stamp: 'dtl 8/19/2022 22:21'!
isStdErr: anIOHandle
	"Answer true if anIOHandle represents stderr."

	| realHandle |
	anIOHandle ifNil: [^ false].
	realHandle _ self processAccessor getStdErrHandle.
	realHandle ifNil: [^ false].
	^ anIOHandle = realHandle
! !

!ThisOSProcess methodsFor: 'private - IOHandle' stamp: 'dtl 8/19/2022 22:21'!
isStdIn: anIOHandle
	"Answer true if anIOHandle represents stdin."

	| realHandle |
	anIOHandle ifNil: [^ false].
	realHandle _ self processAccessor getStdInHandle.
	realHandle ifNil: [^ false].
	^ anIOHandle = realHandle
! !

!ThisOSProcess methodsFor: 'private - IOHandle' stamp: 'dtl 8/19/2022 22:21'!
isStdOut: anIOHandle
	"Answer true if anIOHandle represents stdout."

	| realHandle |
	anIOHandle ifNil: [^ false].
	realHandle _ self processAccessor getStdOutHandle.
	realHandle ifNil: [^ false].
	^ anIOHandle = realHandle
! !

!ThisOSProcess methodsFor: 'initialize - release' stamp: 'dtl 7/26/2010 13:51'!
initialize
	"Set my instance variables to reflect the state of the OS process in which 
	this Smalltalk virtual machine is. executing."

	accessProtect _ Semaphore forMutualExclusion.
	self initializeAllMyChildren.
	processAccessor ifNotNil:
		[processAccessor breakDependents.
		processAccessor _ nil].
	((self processAccessor notNil
		and: [processAccessor canAccessSystem])
			and: [pid ~= processAccessor primGetPid])
		ifTrue: [self resetChildProcessDictionary]
! !

!ThisOSProcess methodsFor: 'initialize - release' stamp: 'dtl 2/4/2015 23:35'!
initializeAllMyChildren
	"Use a Dictionary if process identifiers are unique. On Windows, the
	process ID is not unique, so use an OrderedCollection instead."

	^ childProcessList _ OrderedCollection new
! !

!ThisOSProcess methodsFor: 'platform identification' stamp: 'dtl 10/10/2001 21:24'!
isResponsibleForThisPlatform
	"Answer true is this is an instance of the class which is responsible for representing
	the OS process for the Squeak VM running on the current platform. A false answer is
	usually the result of running the image on a different platform and VM."

	^ self subclassResponsibility! !

!ThisOSProcess methodsFor: 'updating' stamp: 'dtl 2/26/2002 08:37'!
needsRefresh
	"Answer true if the sessionID variable is out of date with respect to the running
	OS Process. Subclasses should provide implementation, answer true as default."

	^ true! !

!ThisOSProcess methodsFor: 'updating' stamp: 'dtl 2/26/2002 08:32'!
refreshFromProcessAccessor
	"Set my instance variables to reflect the state of the OS process in which this Smalltalk
	virtual machine is executing."

	self subclassResponsibility! !

!ThisOSProcess methodsFor: 'updating' stamp: 'dtl 2/27/2015 07:25'!
resetChildProcessDictionary
	"Forget all the entries in the allMyChildren dictionary. This method may be called
	when a new session is started, since the child processes of the previous session are
	no longer children of this process."

	self allMyChildren do: [ :p | self unregisterChildProcess: p ]
! !

!ThisOSProcess methodsFor: 'updating' stamp: 'dtl 10/15/2001 21:27'!
update: aParameter

	aParameter == #invalidProcessAccessor ifTrue: [processAccessor _ nil].
	^ super update: aParameter! !

!ThisOSProcess methodsFor: 'accessing' stamp: 'dtl 2/28/2002 13:30'!
processAccessor

	| a |
	processAccessor
		ifNil: 
			[a _ OSProcessAccessor forThisOSProcess.
			a isResponsibleForThisPlatform ifTrue:
				[processAccessor _ a.
				processAccessor addDependent: self]].
	^ processAccessor! !

!ThisOSProcess methodsFor: 'accessing' stamp: 'dtl 9/26/2005 20:04'!
sessionID

	^ sessionID! !

!ThisOSProcess methodsFor: 'accessing' stamp: 'dtl 9/25/2005 13:30'!
stdErr

	^ stdErr! !

!ThisOSProcess methodsFor: 'accessing' stamp: 'dtl 9/25/2005 13:30'!
stdIn

	^ stdIn! !

!ThisOSProcess methodsFor: 'accessing' stamp: 'dtl 9/25/2005 13:31'!
stdOut

	^ stdOut! !

!ThisOSProcess methodsFor: 'private' stamp: 'dtl 8/20/2022 00:47'!
setStdErr
	"If stdErr is nil, then set it. If not nil, check to see if it is has a valid connection to
	stderr. If not valid, then replace it, otherwise answer the existing valid stream.
	Obscure bug warning: If a valid AttachableFileStream on stderr is garbage collected,
	then stderr will be closed. It is advisable (but not necessary) to treat the stream
	on stderr as a singleton, but in any case, any extra instances attached to stderr
	should not be allowed to be garbage collected."

	| stdErrHandle |
	stdErr ifNotNil:
			[(self isStdErr: stdErr fileID) ifTrue: [^ stdErr]].
	stdErrHandle _ self processAccessor getStdErrHandle.
	stdErrHandle ifNotNil:
			[stdErr _ AttachableFileStream name: 'stderr' attachTo: stdErrHandle writable: true].
	^ stdErr
! !

!ThisOSProcess methodsFor: 'private' stamp: 'dtl 8/20/2022 00:48'!
setStdIn
	"If stdIn is nil, then set it. If not nil, check to see if it is has a valid connection to
	stdin. If not valid, then replace it, otherwise answer the existing valid stream.
	Obscure bug warning: If a valid AttachableFileStream on stdin is garbage collected,
	then stdin will be closed. It is advisable (but not necessary) to treat the stream
	on stdin as a singleton, but in any case, any extra instances attached to stdin
	should not be allowed to be garbage collected."

	| stdInHandle |
	stdIn ifNotNil:
			[(self isStdIn: stdIn fileID) ifTrue: [^ stdIn]].
	stdInHandle _ self processAccessor getStdInHandle.
	stdInHandle ifNotNil:
			[stdIn _ AttachableFileStream name: 'stdin' attachTo: stdInHandle writable: false].
	^ stdIn

! !

!ThisOSProcess methodsFor: 'private' stamp: 'dtl 8/20/2022 00:48'!
setStdOut
	"If stdOut is nil, then set it. If not nil, check to see if it is has a valid connection to
	stdout. If not valid, then replace it, otherwise answer the existing valid stream.
	Obscure bug warning: If a valid AttachableFileStream on stdout is garbage collected,
	then stdout will be closed. It is advisable (but not necessary) to treat the stream
	on stdout as a singleton, but in any case, any extra instances attached to stdout
	should not be allowed to be garbage collected."

	| stdOutHandle |
	stdOut ifNotNil:
			[(self isStdOut: stdOut fileID) ifTrue: [^ stdOut]].
	stdOutHandle _ self processAccessor getStdOutHandle.
	stdOutHandle ifNotNil:
			[stdOut _ AttachableFileStream name: 'stdout' attachTo: stdOutHandle writable: true].
	^ stdOut
! !

!ThisOSProcess methodsFor: 'testing' stamp: 'dtl 12/5/2020 11:36'!
isExternalProcess

	^ false! !

!ThisOSProcess class methodsFor: 'system startup' stamp: 'dtl 6/25/2020 10:26'!
shutDown: quitting
	"Break dependency on my OSProcessAccessor. This is done explicitly at
	shutDown time in order to prevent possible problems when an image is
	restarted on another platform type, in which case a new ThisOSProcess
	instance is created and the old instance could still have an unwanted
	dependency on an OSProcessAccessor."

	quitting
		ifTrue: [ self thisOSProcess ifNotNil: [ :osp |
			osp processAccessor ifNotNil: [ :acc |
				acc breakDependents ] ] ]
! !

!ThisOSProcess class methodsFor: 'system startup' stamp: 'dtl 2/6/2015 20:19'!
startUp: resuming
	"Initialize my singleton instance, and the singleton instance of my
	OSProcessAccessor. On Unix, set the signal handler in my process
	accessor to respond to externally generated sigchld signals. This
	must be done after each image restart in order to call a primitive
	which informs the VM of the identity of the semaphore to signal.
	When not running on a Unix system, the primitive fails and the
	method has no effect. Notify dependents of the singleton instance
	if the image has restarted in a different OS process (this is not the
	case when #startUp is called after a simple image save). The
	notification is done in the initialization of my OSProcessAccessor."

	| aio |
	(aio _ Smalltalk at: #AioEventHandler)
		ifNotNil: [ aio startUp: resuming ].
	resuming ifTrue: [ self initializeThisOSProcess ]! !

!ThisOSProcess class methodsFor: 'concrete subclasses' stamp: 'dtl 3/5/2005 12:08'!
concreteClass

	"ThisOSProcess concreteClass"

	^ self subclasses
		detect: [:c | c isResponsibleForThisPlatform]
		ifNone: [self notify: self printString,
					': No concrete class implementation available for system type ',
					self platformName printString.
				nil]

! !

!ThisOSProcess class methodsFor: 'concrete subclasses' stamp: 'dtl 6/28/2010 21:14'!
concreteClassOrNil

	"ThisOSProcess concreteClassOrNil"

	^ self subclasses
		detect: [:c | c isResponsibleForThisPlatform]
		ifNone: [nil]

! !

!ThisOSProcess class methodsFor: 'initialize-release' stamp: 'dtl 2/18/2016 21:20'!
initialize
	"ThisOSProcess initialize"

	OSProcess initialize.	"required to ensure the change sets file in smoothly"
	AttachableFileStream initialize.
	OSProcessAccessor initialize.
	ChildListSize _ 20.	"list will be pruned to this size, except for any children still running"
	self initializeThisOSProcess.
	self isPharo5Update50558AndLater
		ifFalse: [ Smalltalk addToStartUpList: ThisOSProcess.
			Smalltalk addToShutDownList: ThisOSProcess ]
		ifTrue:
			[ (Smalltalk at: #SessionManager) default
				perform: #registerToolClassNamed:
				with: ThisOSProcess name ]
! !

!ThisOSProcess class methodsFor: 'initialize-release' stamp: 'dtl 8/25/2010 22:58'!
initializeThisOSProcess
	"Initialize the singleton instance, creating a new instance only if the
	platform type has changed since shutdown (running on a different
	type of computer)."

	(ThisInstance isNil or: [ThisInstance isResponsibleForThisPlatform not])
		ifTrue: [ | cls |
			(cls _ self concreteClassOrNil)
				ifNil: [ThisInstance _ nil "no concrete class for this platform"]
				ifNotNil: [ThisInstance _ cls basicNew]].
	ThisInstance initialize! !

!ThisOSProcess class methodsFor: 'initialize-release' stamp: 'dtl 7/5/2010 11:56'!
initializeThisOSProcessWithNotifier
	"Initialize the singleton instance, creating a new instance only if the
	platform type has changed since shutdown (running on a different
	type of computer). Warn if no implementation is available for this platform."

	(ThisInstance isNil or: [ThisInstance isResponsibleForThisPlatform not])
		ifTrue: [ThisInstance _ self concreteClass basicNew].
	ThisInstance initialize! !

!ThisOSProcess class methodsFor: 'testing' stamp: 'dtl 8/18/2014 20:08'!
isHeadless
	"Answer true if the image is known to be headless, otherwise assume
	that a user interface is present."

	(Smalltalk respondsTo: #isHeadless)
		ifTrue: [ ^Smalltalk perform: #isHeadless ].
	^ false "assume that a UI is present"! !

!ThisOSProcess class methodsFor: 'instance creation' stamp: 'dtl 11/5/2000 16:10'!
new

	self notify: self name, ': Only one instance of ThisOSProcess or any of its subclasses should exist in the image. Use #thisOSProcess to obtain the singleton instance.'.
	self shouldNotImplement! !

!ThisOSProcess class methodsFor: 'instance creation' stamp: 'dtl 10/1/2006 08:38'!
thisOSProcess
	"Answer a single instance of the class corresponding to the OS process in 
	which this Smalltalk image is executing."

	"ThisOSProcess thisOSProcess"

	^ ThisInstance! !

!MacProcess methodsFor: 'child process creation' stamp: 'dtl 10/18/2001 20:18'!
command: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in
	the standard C library, except that aCommandString runs asynchronously
	in a child process. Answer an instance of ExternalMacOSProcess which
	is a proxy for the new MacOS process."

	self notYetImplemented
! !

!MacProcess methodsFor: 'child process creation' stamp: 'dtl 10/18/2001 20:18'!
forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams
	"Use my processAccessor to call vfork() and execve() and create a
	new child task. Answer a proxy for the new task, an instance of
	ExternalMacOSProcess."

	self notYetImplemented
! !

!MacProcess methodsFor: 'environment' stamp: 'dtl 1/23/2013 21:10'!
getCwd
	"Not yet implemented - answer a reasonable default."

	^ OSProcess defaultPathString! !

!MacProcess methodsFor: 'initialize - release' stamp: 'dtl 10/14/2001 14:03'!
initialize
	"Set my instance variables to reflect the state of the OS process in which 
	this Smalltalk virtual machine is executing."
! !

!MacProcess methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:46'!
isResponsibleForThisPlatform
	"Answer true is this is an instance of the class which is responsible for representing
	the OS process for the Squeak VM running on the current platform. A false answer is
	usually the result of running the image on a different platform and VM."

	^ self class isNonUnixMac! !

!MacProcess class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:46'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for
	the Squeak VM running on the current platform."

	^ self isNonUnixMac
! !

!OS2Process methodsFor: 'child process creation' stamp: 'dtl 10/18/2001 20:18'!
command: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in
	the standard C library, except that aCommandString runs asynchronously
	in a child process. Answer an instance of ExternalOS2OSProcess which
	is a proxy for the new OS2 process."

	self notYetImplemented
! !

!OS2Process methodsFor: 'child process creation' stamp: 'dtl 10/18/2001 20:18'!
forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams
	"Use my processAccessor to call vfork() and execve() and create a
	new child task. Answer a proxy for the new task, an instance of
	ExternalOS2OSProcess."

	self notYetImplemented
! !

!OS2Process methodsFor: 'environment' stamp: 'dtl 1/23/2013 21:09'!
getCwd
	"Not yet implemented - answer a reasonable default."

	^ OSProcess defaultPathString! !

!OS2Process methodsFor: 'initialize - release' stamp: 'dtl 10/14/2001 14:03'!
initialize
	"Set my instance variables to reflect the state of the OS process in which 
	this Smalltalk virtual machine is executing."
! !

!OS2Process methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:47'!
isResponsibleForThisPlatform
	"Answer true is this is an instance of the class which is responsible for representing
	the OS process for the Squeak VM running on the current platform. A false answer is
	usually the result of running the image on a different platform and VM."

	^ self class isOS2
! !

!OS2Process class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:47'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for
	the Squeak VM running on the current platform."

	^ self isOS2
! !

!RiscOSProcess methodsFor: 'child process creation' stamp: 'dtl 10/18/2001 20:18'!
command: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in
	the standard C library, except that aCommandString runs asynchronously
	in a child process. Answer an instance of ExternalRiscOSProcess which
	is a proxy for the new RiscOS task."

	self notYetImplemented
! !

!RiscOSProcess methodsFor: 'child process creation' stamp: 'dtl 10/18/2001 20:18'!
forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams
	"Use my processAccessor to call vfork() and execve() and create a
	new child task. Answer a proxy for the new task, an instance of
	ExternalRiscOSProcess."

	self notYetImplemented
! !

!RiscOSProcess methodsFor: 'environment' stamp: 'dtl 1/23/2013 21:10'!
getCwd
	"Not yet implemented - answer a reasonable default."

	^ OSProcess defaultPathString! !

!RiscOSProcess methodsFor: 'initialize - release' stamp: 'dtl 10/14/2001 14:04'!
initialize
	"Set my instance variables to reflect the state of the OS process in which 
	this Smalltalk virtual machine is executing."
! !

!RiscOSProcess methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:48'!
isResponsibleForThisPlatform
	"Answer true is this is an instance of the class which is responsible for representing
	the OS process for the Squeak VM running on the current platform. A false answer is
	usually the result of running the image on a different platform and VM."

	^ self class isRiscOS
! !

!RiscOSProcess class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:48'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for
	the Squeak VM running on the current platform."

	^ self isRiscOS
! !

!RiscOSProcess class methodsFor: 'utility' stamp: 'dtl 3/11/2001 11:47'!
makeVmIn: buildDirectoryPathName
	"Rebuild the virtual machine and plugins in the buildDirectoryPathName
	directory. If the build is successful, save the image and restart
	using the new VM. This assumes that the currently executing VM is
	either located in, or linked to, the buildDirectoryPathName directory."
! !

!UnixProcess methodsFor: 'accessing' stamp: 'dtl 11/7/2000 09:09'!
arguments

	^ arguments
! !

!UnixProcess methodsFor: 'accessing' stamp: 'dtl 11/7/2000 09:09'!
environment

	^ environment
! !

!UnixProcess methodsFor: 'accessing' stamp: 'dtl 11/7/2000 09:10'!
path

	^ path _ self pathString
! !

!UnixProcess methodsFor: 'accessing' stamp: 'dtl 11/7/2000 09:10'!
path: aPathString

	self environmentAt: #PATH put: aPathString.
	path _ self pathString! !

!UnixProcess methodsFor: 'accessing' stamp: 'dtl 10/18/2001 20:01'!
pid

	^ pid _ self processAccessor primGetPid
! !

!UnixProcess methodsFor: 'accessing' stamp: 'dtl 11/7/2000 09:10'!
ppid
	"Always refresh ppid from the processAccessor, because it is possible for a child
	to be reparented when the parent exits. The child does not know about this, so
	we refresh ppid on every access."

	^ ppid _ processAccessor primGetPPid
! !

!UnixProcess methodsFor: 'accessing' stamp: 'dtl 11/7/2000 09:10'!
programName

	^ programName
! !

!UnixProcess methodsFor: 'accessing' stamp: 'dtl 3/17/2007 22:54'!
pthread
	"The identity of the pthread in which the interpreter executes.
	Always refresh pthread from the processAccessor, because it is possible for a child
	to be reparented when the parent exits. The child does not know about this, so
	we refresh pthread on every access."

	^ pthread _ processAccessor getThreadID
! !

!UnixProcess methodsFor: 'printing' stamp: 'dtl 7/7/2010 07:37'!
printOn: aStream
	"In English, say 'a Unix' rather than 'an Unix'. Therefore do not use super printOn, which
	treats $U as a vowel."

	self ppid.		"Force update of ppid, in case parent may have exited."
	aStream nextPutAll: 'a ';
			nextPutAll: self class name;
			nextPutAll: ' with pid ';
			nextPutAll: self pid printString
! !

!UnixProcess methodsFor: 'private' stamp: 'dtl 12/18/2022 17:24'!
argsAsFlatArrayAndOffsets: anArrayOfNullTerminatedStrings
	"Given anArrayOfNullTerminatedStrings, flatten the array into a string buffer,
	leaving space at the beginning of the buffer for a list of C pointers. Answer
	the string buffer and an array of address offsets. The address offsets may later
	be converted to C pointers and overlaid on the beginning of the buffer, resulting
	in a data structure which can be treated as a C array of strings suitable for
	use as a Unix argv vector."

	"OSProcess thisOSProcess argsAsFlatArrayAndOffsets:
		(UnixProcess arguments collect:
			[:e | e, (Character null) asString])"

	| offset arraySize flatStringArray ws addresses stringBytes |
	stringBytes _ anArrayOfNullTerminatedStrings collect: [ :e | e asByteArray].
	"Preserve offset space to hold address pointers."
	offset _ (stringBytes size + 1) * self processAccessor sizeOfPointer.
	"Allocate flat strings buffer"
	arraySize _ (stringBytes collect: [:e | e size]) sum
		+ ((stringBytes size + 1) * self processAccessor sizeOfPointer).
	"Flatten the strings out into a buffer, leaving room at the
	beginning of the buffer for an array of addresses."
	flatStringArray _ ByteArray new: arraySize.
	ws _ WriteStream on: flatStringArray.
	offset timesRepeat: [ws nextPut: 0].
	stringBytes do: [:e | ws nextPutAll: e ].
	"Find address offsets to be used in creating the addresses for the strings."
	ws _ WriteStream on: (Array new: stringBytes size).
	(stringBytes collect: [:e | e size])
		inject: offset into: [:p :e | ws nextPut: p. p + e].
	addresses _ ws contents.
	"Results"
	^ Array with: flatStringArray with: addresses
! !

!UnixProcess methodsFor: 'private' stamp: 'dtl 12/15/2023 20:28'!
envAsFlatArrayAndOffsets: anEnvironmentDictionary
	"Given anEnvironmentDictionary, flatten the dictionary into a string buffer,
	leaving space at the beginning of the buffer for a list of C pointers. Answer
	the string buffer and an array of address offsets. The address offsets may later
	be converted to C pointers and overlaid on the beginning of the buffer, resulting
	in a data structure which can be treated as a C array of strings suitable for
	use as a Unix process environment."

	"OSProcess thisOSProcess envAsFlatArrayAndOffsets: UnixProcess env"

	| offset envArray arraySize flatStringArray ws addresses |
	"Preserve offset space to hold address pointers."
	offset _ (anEnvironmentDictionary size + 1) * self processAccessor sizeOfPointer.
	"Build collection of environment strings"
	envArray _ OrderedCollection new.
	anEnvironmentDictionary keysAndValuesDo: [:k :v | envArray add: (k, '=', v) asByteArray].
	"Allocate flat strings buffer"
	arraySize _ ((envArray collect: [:e | e size + 1]) sum) + offset.
	"Flatten the strings out into a buffer, leaving room at the beginning of
	the buffer for an array of addresses."
	flatStringArray _ ByteArray new: arraySize.
	ws _ WriteStream on: flatStringArray.
	offset timesRepeat: [ws nextPut: 0].
	envArray do: [:e | ws nextPutAll: e; nextPut: 0].
	"Find address offsets to be used in creating the addresses for the strings."
	ws _ WriteStream on: (Array new: anEnvironmentDictionary size).
	(envArray collect: [:e | e size + 1])
		inject: offset into: [:p :e | ws nextPut: p. p + e].
	addresses _ ws contents.
	"Results"
	^ Array with: flatStringArray with: addresses
! !

!UnixProcess methodsFor: 'private' stamp: 'dtl 3/31/2001 14:16'!
getArgumentList
	"Answer the argument list using anOSProcessAccessor. For Unix, the first element of the list
	would be the program name. This element will not be treated as an argument; rather, it is
	stored as the programName instance variable."

	| index val list |
	list _ OrderedCollection new.
	index _ 2.
	[(val _ processAccessor primArgumentAt: index) notNil] whileTrue: [
		list add: val.
		index _ index + 1].
	^ list asArray

! !

!UnixProcess methodsFor: 'private' stamp: 'dtl 3/31/2001 14:14'!
getEnvironmentDictionary
	"Answer an environment dictionary using an OSProcessAccessor."

	| index str key val env |
	env _ Dictionary new.
	index _ 1.
	[(str _ processAccessor primEnvironmentAt: index) notNil] whileTrue: [
		key _ (str copyUpTo: $=) asSymbol.
		val _ (str copyFrom: ((str indexOf: $=) + 1) to: (str size)).
		env at: key put: val.
		index _ index + 1].
	^ env


! !

!UnixProcess methodsFor: 'private' stamp: 'dtl 3/31/2001 14:16'!
getProgramName
	"Answer the name of the program which is being run by this OS process. Assume the
	Unix convention where the first element of (char **)argv is the program name."

	^ programName _ processAccessor primArgumentAt: 1
! !

!UnixProcess methodsFor: 'private' stamp: 'dtl 11/7/2000 09:26'!
needsRefresh
	"Answer true if the sessionID variable is out of date with respect to the running OS Process."

	^ ((sessionID ~= (self processAccessor primGetSession)) | (pid ~= (self processAccessor primGetPid)))
! !

!UnixProcess methodsFor: 'private' stamp: 'dtl 3/6/2001 21:46'!
pathString
	"Answer the path string from the environment. Assume Unix convention in which the
	path name is a colon delimited string stored in the PATH environment variable."

	^ self environment at: #PATH ifAbsent: [nil]! !

!UnixProcess methodsFor: 'environment' stamp: 'dtl 1/25/2013 19:00'!
chDir: pathString
	"Change current working directory, and update $PWD if it exists in the environment.
	Answer nil for success, or an error message."

	"OSProcess thisOSProcess chDir: '/tmp'"
	"OSProcess thisOSProcess chDir: '/no/such/path'"
	"OSProcess thisOSProcess chDir: OSProcess defaultPathString"

	| realPath result |
	realPath _ self processAccessor realpath: pathString.
	realPath ifNil: [realPath _ pathString].
	result _ self processAccessor chDir: realPath.
	result isNil
		ifTrue:
			[(self environmentAt: #PWD)
				ifNotNil:
					[self environmentAt: #PWD put: realPath.
					^ nil]]
		ifFalse:
			[self inform: realPath, ': ', result.
			^ result]! !

!UnixProcess methodsFor: 'environment' stamp: 'dtl 3/6/2001 21:30'!
environmentAt: aSymbol
	"Answer an environment variable for the external OS process, and update the dictionary
	in this Smalltalk object."

	^ environment at: aSymbol asSymbol ifAbsent: []
! !

!UnixProcess methodsFor: 'environment' stamp: 'dtl 3/6/2001 21:30'!
environmentAt: aSymbol put: aString
	"Set an environment variable for the external OS process, and update the dictionary in
	this Smalltalk object."

	| s |
	self initialize.
	s _ self processAccessor environmentAt: aSymbol put: aString.
	s ifNotNil: [ self environment at: aSymbol asSymbol put: aString ].
	^ s

! !

!UnixProcess methodsFor: 'environment' stamp: 'dtl 3/22/2000 05:55'!
getCwd
	"Get current working directory. At image startup, this is equivalent to
	evaluating environmentAt: #PWD"

	"OSProcess thisOSProcess getCwd"

	^ self processAccessor primGetCurrentWorkingDirectory


! !

!UnixProcess methodsFor: 'external command processing' stamp: 'dtl 2/27/2002 15:24'!
command: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in the standard C library,
	except that aCommandString runs asynchronously in a child process. The command is
	run by a ConnectedUnixProcess in order to facilitate command pipelines within Squeak."

	"UnixProcess thisOSProcess command: 'ls -l /etc'"

	| proc |
	pid isNil
		ifTrue:
			[self class noAccessorAvailable. ^nil]
		ifFalse:
			[proc _ self
					forkJob: ExternalUnixOSProcess defaultShellPath
					arguments: (Array with: '-c' with: aCommandString)
					environment: nil
					descriptors: nil.
			proc ifNil: [self class noAccessorAvailable].
			^ proc]
! !

!UnixProcess methodsFor: 'external command processing' stamp: 'dtl 1/20/2018 20:00:54'!
command: aCommandString input: aStreamOrString
	"Run a command in a shell process. Similar to the system(3) call in the standard C library,
	except that aCommandString runs asynchronously in a child process."

	"OSProcess thisOSProcess
		command: 'cat'
		input: 'this is some test data'"

	"OSProcess thisOSProcess
		command: 'cat'
		input: (ReadStream on: 'this is some test data')"

	| proc |
	(Smalltalk hasClassNamed: #PipeableOSProcess)
		ifTrue:
			[proc _ (Smalltalk at: #PipeableOSProcess) command: aCommandString.
			proc ifNil: [^ nil].
			proc nextPutAll: aStreamOrString contents.
			(proc perform: #pipeToInput) close.
			^ proc]
		ifFalse:
			[self notify: 'the #command:input: method requires CommandShell, using #command: instead'.
			^ self command: aStreamOrString contents]

! !

!UnixProcess methodsFor: 'external command processing' stamp: 'dtl 2/27/2002 15:24'!
waitForCommand: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in the standard C library.
	The active Smalltalk process waits for completion of the external command process. This just
	uses a simple polling loop, which is not very elegant but works well enough for most purposes."

	"OSProcess thisOSProcess waitForCommand: 'echo sleeping...; sleep 3; echo I just slept for three seconds'"

	| proc d |
	d _ Delay forMilliseconds: 50.
	proc _ self
		forkJob: ExternalUnixOSProcess defaultShellPath
		arguments: (Array with: '-c' with: aCommandString)
		environment: nil
		descriptors: nil.
	proc ifNil: [self class noAccessorAvailable].
	[proc runState == #complete] whileFalse: [d wait].
	^ proc
! !

!UnixProcess methodsFor: 'external command processing' stamp: 'dtl 1/20/2018 20:03:54'!
waitForCommandOutput: aCommandString 
	"Run a command in a shell process. Similar to the system(3) call in the 
	standard C library. The active Smalltalk process waits for completion of
	the external command process."

	"OSProcess thisOSProcess waitForCommandOutput: 'echo sleeping...; sleep 1; echo I just slept for one second'"
	"OSProcess thisOSProcess waitForCommandOutput: 'ThisIsABogusCommand'"
	"OSProcess thisOSProcess waitForCommandOutput: '/bin/ls -l /etc /bin'"
	"OSProcess thisOSProcess waitForCommandOutput: 'echo Hello world!!; ls /NOSUCHFILE'"

	(Smalltalk hasClassNamed: #PipeableOSProcess)
		ifTrue:
			[^ ((Smalltalk at: #PipeableOSProcess) command: aCommandString) perform: #output]
		ifFalse:
			[self notify: 'the #waitForCommandOutput: method requires CommandShell'.
			^ '']
! !

!UnixProcess methodsFor: 'external command processing' stamp: 'dtl 1/20/2018 20:03:32'!
waitForCommandOutputArray: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in the standard C library.
	The active Smalltalk process waits for completion of the external command process."

	"OSProcess thisOSProcess waitForCommandOutputArray: 'echo Hello world!!; ls /NOSUCHFILE'"

	| proc |
	(Smalltalk hasClassNamed: #PipeableOSProcess)
		ifTrue:
			[proc _ (Smalltalk at: #PipeableOSProcess) command: aCommandString.
			^ Array
				with: (proc perform: #output)
				with: (proc perform: #errorUpToEnd)
				with: (proc perform: #processProxy) exitStatus]
		ifFalse:
			[self notify: 'the #waitForCommandOutputArray: method requires CommandShell'.
			^ Array with: '' with: '' with: nil]
! !

!UnixProcess methodsFor: 'finalization' stamp: 'dtl 2/4/2015 23:39'!
finalize
	"Use this to release any external resources prior to reinitializing."

	super finalize.
	stdIn _ stdIn ifNotNil:
		[[stdIn close] on: Error do: [:ex | ].
		nil].
	stdIn _ stdOut ifNotNil:
		[[stdOut close] on: Error do: [:ex | ].
		nil].
	stdIn _ stdErr ifNotNil:
		[[stdErr close] on: Error do: [:ex | ].
		nil].
	sessionID _ nil.
	ppid _ nil.
	programName _ nil.
	arguments _ nil.
	path _ nil.
	environment _ nil.
	self updateAllMyChildren.
	childProcessList _ nil.
	processAccessor
		ifNotNil: 
			[processAccessor removeDependent: self.
			processAccessor _ nil]
! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 2/27/2002 15:24'!
forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams 

	"Call Unix vfork() and execve() to create a child process, and answer the 
	child process. This method is expected to be called by class side methods."

	^ ExternalUnixOSProcess
		forkAndExec: executableFile
		arguments: arrayOfStrings
		environment: stringDictionary
		descriptors: arrayOf3Streams! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 2/27/2015 20:04'!
forkHeadlessSqueak
	"Just like forkSqueak, except that the child Squeak continues headless."

	| thisPid childPid child connected |
	stdOut ifNil: [^ nil].
	self stdOut flush.
	self stdErr flush.
	thisPid _ self pid.
	connected _ self processAccessor canControlXDisplay
					and: [self flushXDisplay notNil].
	childPid _ self processAccessor forkSqueak.
	^ childPid ifNotNil: [
		childPid == 0
			ifTrue:
				[connected ifTrue: [self disconnectXDisplay].
				OSProcess thisOSProcess processAccessor newPid.
				^ self]
			ifFalse:
				[child _ ExternalUnixOSProcess new.
				child pid: childPid.
				child ppid: thisPid.
				child programName: self programName.
				child initialStdIn: self stdIn.	
				child initialStdOut: self stdOut.
				child initialStdErr: self stdErr.
				child arguments: self arguments.
				child initialEnvironment: self environment.
				child notYetRunning.
				self registerChildProcess: child.
				child running.
				^ child]]
! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 1/7/2001 12:59'!
forkHeadlessSqueakAndDo: aBlock 
	"Start a new instance of Squeak running in a child OS process, and  
	execute aBlock in the child instance. The new instance is a clone of 
	this image, but without a connection to the X display. The child instance 
	executes aBlock, which hopefully does not involve interaction with the 
	X display; and the parent continues normally.  
	  
	The child should not depend on using existing connections to external  
	resources. For example, the child may lose its connections to stdin, 
	stdout, and stderr after its parent exits."

	| childOrThisProc |
	childOrThisProc _ self forkHeadlessSqueak.
	childOrThisProc
		ifNil: 
			[self class noAccessorAvailable.
			^ nil].
	childOrThisProc == self
		ifTrue:
			["Child process"
			aBlock value].
	^ childOrThisProc
! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 8/7/2003 07:17'!
forkHeadlessSqueakAndDoThenQuit: aBlock 
	"Start a new instance of Squeak running in a child OS process, and 
	execute aBlock in the child instance. The new instance is a clone of
	this image, but without a connection to the X display. The child instance
	executes aBlock, which hopefully does not involve interaction with the
	X display; and the parent continues normally.

	The child should not depend on using existing connections to external 
	resources. For example, the child may lose its connections to stdin, stdout,
	and stderr after its parent exits."

	"self thisOSProcess forkHeadlessSqueakAndDoThenQuit:
		[OSProcess thisOSProcess stdOut nextPutAll: 'hello world!!'; nextPut: Character lf]"
	"self thisOSProcess forkHeadlessSqueakAndDoThenQuit: [OSProcess thisOSProcess command: 'xeyes']"

	| childOrThisProc |
	childOrThisProc _ self forkHeadlessSqueak.
	childOrThisProc
		ifNil: 
			[self class noAccessorAvailable.
			^ nil].
	childOrThisProc == self
		ifTrue:
			["Child process"
			aBlock value.
			Smalltalk quitPrimitive].
	^ childOrThisProc! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 12/27/2000 16:47'!
forkJob: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams
	"Call Unix vfork() and execve() to create a child process, and answer the child process.
	Delegate this to the singleton OSProcess>>thisOSProcess."

	^ self forkAndExec: executableFile
		arguments: arrayOfStrings
		environment: stringDictionary
		descriptors: arrayOf3Streams
! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 2/27/2015 20:04'!
forkSqueak
	"Fork a child and continue running this Squeak image in both the parent and the child.
	Parent and child are distinguished by the pid returned by primForkSqueak.

	If continuing as the parent process, answer the ExternalUnixOSProcess which
	represents the child. This can be inspected to watch the run state of the child
	process from the parent.

	If continuing as the child process, answer OSProcess thisOSProcess. This can be
	inspected to watch the full state of the child process from the child. The
	child cannot directly view the state of its parent.

	Parent and child should be cautious about using shared connections to external
	resources."

	"self thisOSProcess forkSqueak"

	| thisPid childPid child connected |
	stdOut ifNil: [^ nil].
	self stdOut flush.
	self stdErr flush.
	thisPid _ self pid.
	connected _ self processAccessor canControlXDisplay
					and: [self flushXDisplay notNil].
	childPid _ self processAccessor forkSqueak.
	^ childPid ifNotNil: [
		childPid == 0
			ifTrue:
				[connected ifTrue: [self disconnectXDisplay; recapitate].
				OSProcess thisOSProcess processAccessor newPid.
				^ self]
			ifFalse:
				[child _ ExternalUnixOSProcess new.
				child pid: childPid.
				child ppid: thisPid.
				child programName: self programName.
				child initialStdIn: self stdIn.	
				child initialStdOut: self stdOut.
				child initialStdErr: self stdErr.
				child arguments: self arguments.
				child initialEnvironment: self environment.
				child notYetRunning.
				self registerChildProcess: child.
				child running.
				^ child]]
! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 10/8/2001 19:56'!
forkSqueakAndDo: aBlock
	"Start a new instance of Squeak running in a child OS process. The new instance is a
	clone of this image except for the return value of this method. It does not reload the
	image file from disk. The child image evaluates aBlock.

	The child should not depend on using existing connections to external resources. For
	example, the child may lose its connections to stdin, stdout, and stderr after its parent
	exits."

	"UnixProcess thisOSProcess forkSqueakAndDo:
		[Object inform: 'Hi, I am the child Squeak process.']"

	| childOrThisProc |
	childOrThisProc _ self forkSqueak.
	(childOrThisProc == self) ifTrue: [aBlock value]. "Child process"
	^ childOrThisProc! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 10/8/2001 19:56'!
forkSqueakAndDoThenQuit: aBlock
	"Start a new instance of Squeak running in a child OS process. The new instance is a
	clone of this image except for the return value of this method. It does not reload the
	image file from disk. The child image evaluates aBlock.

	The child should not depend on using existing connections to external resources. For
	example, the child may lose its connections to stdin, stdout, and stderr after its parent
	exits."

	"UnixProcess thisOSProcess forkSqueakAndDoThenQuit:
		[Object inform: 'Hi, I am the child Squeak process.']"

	| childOrThisProc |
	childOrThisProc _ self forkSqueak.
	(childOrThisProc == self)
		ifTrue:
			[ aBlock value.
			Smalltalk quitPrimitive]. "Child process"
	^ childOrThisProc! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 12/18/2022 17:22'!
processProxy: anExternalProcess forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams
	"Call Unix vfork() and execve() to create a child process, and answer the child process.
	This method is expected to be called by class side methods. Prepare the arguments before
	calling the primitive, including null termination of all strings. anExternalProcess is an
	object which represents the new child process, and which responds to the #pid: message."

	| nullString progName args argVecAndOffsets argVec argOffsets
	envVecAndOffsets envVec envOffsets in out err childPid pwd |
	stdOut ifNil: [^ nil].
	nullString _ (Character null) asString.
	progName _ (executableFile, nullString) asByteArray. "Null terminated string"
	arrayOfStrings isNil							"Should be a (possibly empty) array"
		ifTrue:
			[args _ Array with: progName]		"First argument is the program name (Unix convention)"
		ifFalse:
			[args _ (OrderedCollection new)
						add: progName;
						addAll: (arrayOfStrings collect: [:e | e, nullString ]);	"Null terminate each string"
						yourself;
						asArray].
	argVecAndOffsets _ self argsAsFlatArrayAndOffsets: args.
	argVec _ argVecAndOffsets at: 1.
	argOffsets _ argVecAndOffsets at: 2.
	(stringDictionary notNil and: [stringDictionary ~= (self environment)])
		ifTrue:
			[envVecAndOffsets _ self envAsFlatArrayAndOffsets: stringDictionary.
			envVec _ envVecAndOffsets at: 1.
			envOffsets _ envVecAndOffsets at: 2]
		ifFalse:
			[envVec _ nil.
			envOffsets _ nil].	"Same as current environment, so just pass nil."
	arrayOf3Streams isNil
		ifTrue:
			[in _ self stdIn fileID.
			out _ self stdOut fileID.
			err _ self stdIn fileID]
		ifFalse:
			[(arrayOf3Streams at: 1)
				isNil
					ifTrue: [in _ self stdIn fileID]
					ifFalse: [in _ (arrayOf3Streams at: 1) fileID].			
			(arrayOf3Streams at: 2)
				isNil
					ifTrue: [ out _ self stdOut fileID]
					ifFalse: [ out _ (arrayOf3Streams at: 2) fileID].
			(arrayOf3Streams at: 3)
				isNil
					ifTrue: [ err _ self stdErr fileID]
					ifFalse: [ err _ (arrayOf3Streams at: 3) fileID]].
	pwd _ anExternalProcess pwd.
	(pwd = self getCwd)
		ifTrue: [pwd _ nil]
		ifFalse: [pwd _ pwd, nullString asByteArray].
	childPid _ self processAccessor
				forkAndExec: progName
				stdIn: in
				stdOut: out
				stdErr: err
				argBuf: argVec
				argOffsets: argOffsets
				envBuf: envVec
				envOffsets: envOffsets
				workingDir: pwd.
	anExternalProcess pid: childPid.
	anExternalProcess ppid: self pid.
	((childPid == 0) or: [childPid isNil])
		ifTrue:
			[anExternalProcess unknownRunState]
		ifFalse:
			[anExternalProcess running.
			self registerChildProcess: anExternalProcess].
	^ anExternalProcess
! !

!UnixProcess methodsFor: 'child process creation' stamp: 'dtl 1/21/2001 11:47'!
squeak
	"Start a new instance of Squeak running in a child OS process. The new 
	instance will restart from the image file, so it is a clone of this image 
	as it existed at the most recent image save."

	"OSProcess thisOSProcess squeak"

	^ self
		forkJob: self programName
		arguments: self arguments
		environment: nil
		descriptors: nil! !

!UnixProcess methodsFor: 'initialize - release' stamp: 'dtl 7/6/2010 21:55'!
initialize
	"Set my instance variables to reflect the state of the OS process in which 
	this Smalltalk virtual machine is executing."

	super initialize.
	(self processAccessor notNil and: [processAccessor canAccessSystem])
		ifTrue: 
			[self refreshFromProcessAccessor]
		ifFalse: 
			[stdIn _ nil.
			stdOut _ nil.
			stdErr _ nil].
	processAccessor restartChildWatcherProcess
! !

!UnixProcess methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:49'!
isResponsibleForThisPlatform
	"Answer true is this is an instance of the class which is responsible for representing
	the OS process for the Squeak VM running on the current platform. A false answer is
	usually the result of running the image on a different platform and VM."

	^ self class isUnix
! !

!UnixProcess methodsFor: 'VM atexit' stamp: 'dtl 2/6/2015 20:38'!
killCurrentChildrenAtExit
	"Arrange for the currently active child processes to receive a SIGTERM
	signal then the Squeak VM exits. Each invokation of this method overrides
	the effects of any previous calls."

	"OSProcess thisOSProcess killCurrentChildrenAtExit"

	self processAccessor killOnVmExit: self activeChildren withSignal: nil
! !

!UnixProcess methodsFor: 'IO redirection' stamp: 'dtl 5/18/2009 21:07'!
redirect: aFileStream to: newFileStream

	| oldfd newfd |
	oldfd _ self processAccessor unixFileNumber: aFileStream fileID.
	newfd _ self processAccessor unixFileNumber: newFileStream fileID.
	self processAccessor primUnixFileClose: oldfd.
	^ self processAccessor primDup: newfd to: oldfd
! !

!UnixProcess methodsFor: 'IO redirection' stamp: 'dtl 5/17/2009 23:12'!
redirectStdErrTo: fileStream
	"Redirect the standard error stream to another output stream."

	^ self redirect: self stdErr to: fileStream
! !

!UnixProcess methodsFor: 'IO redirection' stamp: 'dtl 5/17/2009 23:12'!
redirectStdInTo: fileStream
	"Redirect the standard input stream to another input stream."

	^ self redirect: self stdIn to: fileStream
! !

!UnixProcess methodsFor: 'IO redirection' stamp: 'dtl 5/17/2009 23:11'!
redirectStdOutTo: fileStream
	"Redirect the standard output stream to another output stream."

	"| pipe |
	pipe _ OSPipe nonBlockingPipe.
	OSProcess thisOSProcess redirectStdOutTo: pipe writer.
	pipe"
	
	^ self redirect: self stdOut to: fileStream
! !

!UnixProcess methodsFor: 'updating' stamp: 'dtl 7/4/2010 22:35'!
refreshFromProcessAccessor
	"Set my instance variables to reflect the state of the OS process in which this Smalltalk
	virtual machine is executing."

	self needsRefresh ifTrue: [
		sessionID _ self processAccessor getSessionIdentifier.
		pid _ processAccessor primGetPid.
		ppid _ processAccessor primGetPPid.
		pthread _ processAccessor getThreadID.
		self setStdIn.
		self setStdOut.
		self setStdErr.
		programName _ self getProgramName.
		arguments _ self getArgumentList.
		environment _ self getEnvironmentDictionary.
		path _ self pathString.
		self updateAllMyChildren]
! !

!UnixProcess methodsFor: 'updating' stamp: 'dtl 10/20/2001 09:05'!
update: aParameter
	"Framework to update some or all of the instance variables based on external events,
	such as receipt of a sigchd signal when a child process exits."

	(aParameter == (self processAccessor)) ifTrue:
		[^ self refreshFromProcessAccessor; yourself].
	(aParameter == #pid) ifTrue:
		[^ self resetChildProcessDictionary. "Forget children of prior process"].
	(aParameter == #childProcessStatus) ifTrue:
		[^ self updateActiveChildren; changed; yourself].
	(aParameter == #startUp) ifTrue:
		[^ self update: #pid].
	aParameter == #invalidProcessAccessor ifTrue:
		[processAccessor _ processAccessor ifNotNil: [processAccessor removeDependent: self. nil].
		^ self].
	self error: 'Unexpected update parameter'! !

!UnixProcess methodsFor: 'child process management' stamp: 'dtl 2/5/2015 20:06'!
registerChildProcess: anOSProcess

	self processAccessor sigChldSemaphore.
	self processAccessor grimReaperProcess.	 "Start the reaper process if it is not running."
	^ super registerChildProcess: anOSProcess
! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:49'!
sigabrt: anExternalOSProcess
	"Send a SIGABRT signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigabrtTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:49'!
sigalrm: anExternalOSProcess
	"Send a SIGALRM signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigalrmTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:48'!
sigchld: anExternalOSProcess
	"Send a SIGCHLD signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigchldTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:48'!
sigcont: anExternalOSProcess
	"Send a SIGCONT signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigcontTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:48'!
sighup: anExternalOSProcess
	"Send a SIGHUP signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSighupTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:48'!
sigint: anExternalOSProcess
	"Send a SIGINT signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigintTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:48'!
sigkill: anExternalOSProcess
	"Send a SIGKILL signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigkillTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:48'!
sigpipe: anExternalOSProcess
	"Send a SIGPIPE signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigpipeTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:49'!
sigquit: anExternalOSProcess
	"Send a SIGQUIT signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigquitTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:50'!
sigstop: anExternalOSProcess
	"Send a SIGSTOP signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigstopTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:50'!
sigterm: anExternalOSProcess
	"Send a SIGTERM signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigtermTo: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:50'!
sigusr1: anExternalOSProcess
	"Send a SIGUSR1 signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigusr1To: anExternalOSProcess pid! !

!UnixProcess methodsFor: 'OS signal sending' stamp: 'dtl 7/6/2000 16:51'!
sigusr2: anExternalOSProcess
	"Send a SIGUSR2 signal to the external process represented by anExternalOSProcess."

	^ self processAccessor primSendSigusr2To: anExternalOSProcess pid! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 3/10/2001 09:52'!
arguments

	"UnixProcess arguments"

	^ self thisOSProcess arguments
! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 12/27/2000 17:18'!
env
	"Note: The #environment selector has special meaning for classes, so use #env."

	"UnixProcess env"

	^ self thisOSProcess environment
! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 12/27/2000 17:16'!
path
	"UnixProcess path"

	^ self thisOSProcess path
! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 3/10/2001 09:53'!
programName

	"UnixProcess programName"

	^ self thisOSProcess programName
! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 3/17/2002 13:38'!
restartVirtualMachine
	"Fork a new instance and quit this one. This moves the running VM into a new
	OS process, and starts a new X display for the new process. It does not reload the
	VM program text, so this cannot be used to restart the VM after rebuild."

	"UnixProcess restartVirtualMachine"

	| proc |
	proc _ self forkSqueak.
	proc isNil
		ifTrue: 
			[self noAccessorAvailable.
			nil]
		ifFalse:
			[OSProcess thisOSProcess == proc
				ifFalse:
					["Quit if this is the parent process"
					Smalltalk quitPrimitive]].
	^ proc! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 3/10/2001 09:53'!
sessionID

	"UnixProcess sessionID"

	^ self thisOSProcess sessionID
! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 7/13/2003 14:47'!
startSwiki: aSwiki onPort: num loggingTo: aFileName
	"Start a swiki in a headless Squeak image."

	"UnixProcess startSwiki: 'myswiki' onPort: 8081 loggingTo: 'log.txt'"

	| proc |
	(Smalltalk hasClassNamed: #SwikiAction)
		ifTrue:
			[proc _ self forkSqueakAndDo:
				[(Smalltalk at: #SwikiAction) new restore: 'myswiki'.
				(Smalltalk at: #PWS) serveOnPort: num loggingTo: aFileName.
				UnixProcess decapitate].
			proc ifNil: [self noAccessorAvailable].
			^ proc]
		ifFalse:
		[self notify: 'PWS not installed in this image']
! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 12/27/2000 17:12'!
stdErr
	"UnixProcess stdErr"

	^ self thisOSProcess stdErr
! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 12/27/2000 17:11'!
stdIn
	"UnixProcess stdIn"

	^ self thisOSProcess stdIn
! !

!UnixProcess class methodsFor: 'utility' stamp: 'dtl 12/27/2000 17:11'!
stdOut
	"UnixProcess stdOut"

	^ self thisOSProcess stdOut
! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 2/24/2013 10:26'!
backgroundProcessInHeadlessSqueak
	"Demonstrate running a ''batch job'' in a low priority background Squeak,
	with output written to an OSPipe. This Squeak image reads data from the
	pipe and displays it on the Transcript. The background Squeak will write
	enough data to block an OSPipe, so the foreground Squeak loops while
	reading available data up to end of file.

	Open a Transcript, then '' inspect it '' on this method. Process runs
	indefinitely, so terminate the process to end demo."

	"UnixProcess backgroundProcessInHeadlessSqueak"

	^ [[| pipe backgroundJob ws s | 
	pipe _ OSPipe new.
	backgroundJob _ OSProcess thisOSProcess
	forkHeadlessSqueakAndDoThenQuit: [| beers | 
		OSProcess accessor nice: 1. "lower priority of background OS process"
		beers _ [:i | (i < 1 ifTrue: ['no more'] ifFalse: [i asString])
					, ' bottle' , (i = 1 ifTrue: [''] ifFalse: ['s'])].
		(99 to: 1 by: -1) do: [:count | pipe nextPutAll:
			(beers value: count) , ' of beer on the wall, '
			, (beers value: count) , ' of beer' , String cr
			, 'take one down and pass it around, '
			, (beers value: count - 1) , ' of beer on the wall'; cr].
		pipe close].
	WorldState addDeferredUIMessage:
		[Transcript show: backgroundJob asString , ' started'; cr].
	pipe closeWriter. "don't need writer end, close it before the #upToEnd"
	"pipe writer blocks when pipe full, so we need to loop while reading to end "
	ws _ WriteStream on: String new.
	[backgroundJob isComplete] whileFalse:
		[(Delay forMilliseconds: 200) wait.
		(s _ pipe upToEnd) ifNotNil: [ws nextPutAll: s]].
	pipe close.
	WorldState addDeferredUIMessage:
		[Transcript show: backgroundJob asString
			, ' completed, display results in 2 seconds'; cr].
	(Delay forSeconds: 2) wait.
	WorldState addDeferredUIMessage:
		[Transcript show: ws contents.
		Transcript cr; show: 'delay 5 seconds before forking next Squeak job'; cr].
	(Delay forSeconds: 5) wait] repeat]
		forkAt: Processor userBackgroundPriority! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 1/16/2018 00:32'!
catAFile
	"Copy contents of a file to standard output. This demonstrates reassigning stdin to an open FileStream."

	"UnixProcess catAFile"

	| in proc |
	in _ OSProcess readOnlyFileNamed: '/etc/hosts'.
	proc _ self forkJob: '/bin/cat'
			arguments: nil
			environment: nil
			descriptors: (Array with: in with: nil with: nil).
	in close.
	proc ifNil: [self noAccessorAvailable].
	^ proc! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 1/16/2018 00:32'!
catFromFileToFiles
	"Copy contents of a file to another file, with any error messages going 
	to a third file."

	"UnixProcess catFromFileToFiles"

	| in out err proc |
	in _ OSProcess readOnlyFileNamed: '/etc/hosts'.
	out _ OSProcess newFileNamed: '/tmp/deleteMe.out'.
	err _ OSProcess newFileNamed: '/tmp/deleteMe.err'.
	proc _ UnixProcess
				forkJob: '/bin/cat'
				arguments: nil
				environment: nil
				descriptors: (Array
						with: in
						with: out
						with: err).
	in close.
	out close.
	err close.
	proc ifNil: [self noAccessorAvailable].
	^ proc! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 11/14/2000 00:07'!
clientServerDemo
	"Start a new headless instance of Squeak running in a child OS process. The new
	instance is a nearly identical copy of its parent, resuming execution at the same
	point as the parent. The child process will run a TCP server process in the headless
	image, then exit. Test results are written to the Transcript.

	Answer '127 0 0 1' in response to the host address dialog."

	"UnixProcess clientServerDemo"

	| proc |
	proc _ self forkHeadlessSqueakAndDoThenQuit: [Socket remoteTestServerTCP].
	proc ifNotNil: [Socket remoteTestClientTCP].
	^ proc

! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 1/7/2001 12:55'!
clientServerDemo2
	"Start a new instance of Squeak running in a child OS process. The new 
	instance is a nearly identical copy of its parent, resuming execution at
	the same point as the parent. The child process will run a TCP server
	process, then exit. Test results are written to the Transcript. "

	"UnixProcess clientServerDemo2"

	| remotePort remoteHost serverBlock socket buffer n bytesToSend sendBuf
	receiveBuf done bytesSent bytesReceived packetsSent packetsReceived t |

	remotePort _ 54321.
	remoteHost _ 'localhost'.
	serverBlock _ 
			[Transcript clear; cr; show: 'This is the server Squeak session'; cr.
			Socket initializeNetworkIfFail: [^ nil].
			socket _ Socket newTCP.
			socket listenOn: remotePort.
			buffer _ String new: 4000.
			socket waitForConnectionUntil: Socket standardDeadline.
			[socket isConnected]
				whileTrue: [socket dataAvailable
						ifTrue: 
							[n _ socket receiveDataInto: buffer.
							socket sendData: buffer count: n]].
			socket closeAndDestroy].

	(UnixProcess forkSqueakAndDoThenQuit: serverBlock)
		ifNil: [self noAccessorAvailable. ^ nil].
	Transcript cr; show: 'This is the client Squeak session'; cr.
	Transcript show: 'starting client/server TCP test'; cr.
	Transcript show: 'initializing network ... '.
	Socket initializeNetworkIfFail: [^ Transcript show: 'failed'].
	Transcript show: 'ok'; cr.
	socket _ Socket newTCP.
	socket connectTo: (NetNameResolver addressForName: remoteHost) port: remotePort.
	socket waitForConnectionUntil: Socket standardDeadline.
	Transcript show: 'client endpoint created'; cr.
	bytesToSend _ 1000000.
	sendBuf _ String new: 4000 withAll: $x.
	receiveBuf _ String new: 50000.
	done _ false.
	bytesSent _ bytesReceived _ packetsSent _ packetsReceived _ 0.
	t _ Time
				millisecondsToRun: 
					[[done]
						whileFalse: 
							[(socket sendDone and: [bytesSent < bytesToSend])
								ifTrue: 
									[packetsSent _ packetsSent + 1.
									bytesSent _ bytesSent + (socket sendData: sendBuf)].
							socket dataAvailable
								ifTrue: 
									[packetsReceived _ packetsReceived + 1.
									bytesReceived _ bytesReceived + (socket receiveDataInto: receiveBuf)].
							done _ bytesSent >= bytesToSend].
					[bytesReceived < bytesToSend]
						whileTrue: [socket dataAvailable
								ifTrue: 
									[packetsReceived _ packetsReceived + 1.
									bytesReceived _ bytesReceived + (socket receiveDataInto: receiveBuf)]]].
	socket closeAndDestroy.
	Transcript show: 'remoteClient TCP test done; time = ' , t printString; cr.
	Transcript show: packetsSent printString , ' packets, ' , bytesSent printString ,
		' bytes sent (' , (bytesSent * 1000 // t) printString , ' bytes/sec)'; cr.
	Transcript show: packetsReceived printString , ' packets, ' , bytesReceived printString ,
		' bytes received (' , (bytesReceived * 1000 // t) printString , ' bytes/sec)'; cr.
	^ bytesReceived! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 9/9/2000 16:00'!
cloneSqueak
	"Start a new instance of Squeak running in a child OS process. The new instance is a
	nearly identical copy of its parent, resuming execution at the same point as the
	parent, and differentiated only by the return value of this method."

	"UnixProcess cloneSqueak"

	| proc |
	proc _ self forkSqueak.
	proc ifNil: [self noAccessorAvailable].
	^ proc
! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 7/12/2000 04:31'!
eightLeafSqueakTree
	"Clone this squeak three times, resulting in a total of (2 raisedTo: 3) nearly identical
	squeaks. Have a look at the pidArray inspectors and to the debug messages on stdout
	in order to see what is going on. The tree of processes looks like this:

	111
	 +--------+--------+
	 |		 |		 |
	011		101		110
	 +---+	 |
	 |	 |	100
	010	001
		 |
		000
	"

	"UnixProcess eightLeafSqueakTree inspect"

	| depth this pidArray debugString |
	depth _ 3.
	this _ OSProcess thisOSProcess.
	this stdOut ifNil: [self noAccessorAvailable. ^ nil].
	pidArray _ Array new: depth.
	(1 to: depth) do: [ :e | | p pid |
		p _ this forkSqueak.
		pid _ (p == this) ifTrue: [0] ifFalse: [p pid].	"Use Unix fork(2) convention"
		pidArray at: e put: pid].
	debugString _ 'pid ', (this pid printString), ' ppid ', (this ppid printString), ' ',
		(pidArray printString), (Character lf asString).
	this stdOut nextPutAll: debugString.
	^ pidArray! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 7/12/2000 05:11'!
headlessChild
	"Start a new instance of Squeak running in a child OS process. The new
	instance is a nearly identical copy of its parent, resuming execution at the
	same point as the parent, and differentiated only by the return value of
	this method. The child squeak will write a message to standard output,
	then exit."

	"UnixProcess headlessChild"

	| this childBlock |
	this _ OSProcess thisOSProcess.
	childBlock _
		[this stdOut nextPutAll: 'hello world from child process '.
		this pid printOn: OSProcess thisOSProcess stdOut.
		this stdOut nextPut: Character lf ].
	^ self forkHeadlessSqueakAndDoThenQuit: childBlock

! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 7/12/2000 04:34'!
listDirectory
	"Execute a simple command, sending output to standard output."

	"UnixProcess listDirectory"

	| proc |
	proc _ self
			forkJob: '/bin/ls'
			arguments: nil
			environment: nil
			descriptors: nil.
	proc ifNil: [self noAccessorAvailable].
	^ proc! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 7/12/2000 04:35'!
spawnTenHeadlessChildren
	"Spawn ten Squeak children, each of which writes a message to standard 
	output, then exits. Answer an array of pid values for the child 
	processes. "

	"UnixProcess spawnTenHeadlessChildren"

	| childBlock count children this |
	this _ OSProcess thisOSProcess.
	this stdOut ifNil: [self noAccessorAvailable. ^ nil].
	count _ 10.
	children _ Array new: count.
	childBlock _ 
			[this stdOut nextPutAll: 'hello world from child process '.
			this pid printOn: this stdOut.
			this stdOut nextPut: Character lf.
			this stdOut flush].
	(1 to: count)
		do: 
			[:e | 
			OSProcess thisOSProcess stdOut flush.
			children at: e put: (self forkHeadlessSqueakAndDoThenQuit: childBlock)].
	^ children! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 7/12/2000 04:38'!
squeakSqueak
	"Start a new instance of Squeak running in a child OS process. The new instance
	will restart from the image file, so it is a clone of this image as it existed at the
	most recent image save. See cloneSqueak for an example of how to clone the
	running image without going back to the saved image file."

	"UnixProcess squeakSqueak"

	| proc |
	proc _ self squeak.
	proc ifNil: [self noAccessorAvailable].
	^ proc
! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 7/12/2000 04:41'!
testEnvSet
	"Set up a new environment for a child process. Exec a shell to show the 
	environment variables on the terminal standard output. Note that many 
	shells will set other environment variables in addition to those which we
	set up prior to executing the shell."

	"UnixProcess testEnvSet"

	| e proc |
	e _ Dictionary new.
	e at: #KEY1 put: 'value1';
	 at: #KEY2 put: 'value2';
	 at: #KEY3 put: 'value3'.
	proc _ self
			forkJob: '/bin/sh'
			arguments: #('-c' 'env' )
			environment: e
			descriptors: nil.
	proc ifNil: [self noAccessorAvailable].
	^ proc! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 2/11/2001 14:03'!
testPipe
	"Create an OS pipe, write some text to it, and read the text back from the 
	other end of the pipe."

	"UnixProcess testPipe inspect"

	| s p r |
	s _ 'this is some text to write into the pipe'.
	p _ OSPipe new.
	p ifNil: 
		[self noAccessorAvailable.
		^ p].
	p writer nextPutAll: s.
	p writer close.
	r _ p reader next: s size.
	p reader close.
	^ r! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 2/24/2001 16:28'!
testPipeLine
	"Create two OS pipes, and a child OS process with its input connected to 
	one pipe and its  output connected to the other pipe. Write some text to
	the input pipe, and read the resulting output (just echoed back by the
	Unix cat command) back through the output pipe. Send a SIGHUP signal
	to the child process to tell it to exit.

	This test verifies the ability of Squeak to send text to an external OS process
	through a pipe, and read the output text back from another pipe.
	See ConnectedUnixProcess for a more useful implementation."

	"UnixProcess testPipeLine inspect"

	| testString pipe1 pipe2 input output src dest desc result child |
	OSProcess accessor canAccessSystem ifFalse: [^ nil].
	testString _ 'This is the text to write out through one pipe, copy through an external cat command, and then read back in through another pipe.'.
	pipe1 _ OSPipe blockingPipe.
	pipe2 _ OSPipe nonBlockingPipe.
	input _ pipe1 reader.
	output _ pipe2 writer.
	src _ pipe1 writer.
	dest _ pipe2 reader.
	desc _ Array
				with: input
				with: output
				with: nil.
	child _ self
				forkJob: '/bin/cat'
				arguments: nil
				environment: nil
				descriptors: desc.
	input close.
	output close.
	src nextPutAll: testString.
	src close.
	(Delay forSeconds: 1) wait.
	result _ dest next: testString size.
	dest close.
	child sigterm. "Tell the child to exit"
	^ result
! !

!UnixProcess class methodsFor: 'examples' stamp: 'dtl 1/15/2018 22:09:27'!
testRunCommand
	"Run the command 'ls -l /etc /etc/noSuchFile'. The output of the 
	command will be in the file '/tmp/deleteMe.out', and the error output
	will be in '/tmp/deleteMe.err'. "

	"UnixProcess testRunCommand"

	| out err desc args proc |
	out _ OSProcess newFileNamed: '/tmp/deleteMe.out'.
	err _ OSProcess newFileNamed: '/tmp/deleteMe.err'.
	desc _ Array
				with: nil
				with: out
				with: err.
	args _ Array
				with: '-l'
				with: '/etc'
				with: '/etc/noSuchFile'
				with: '/etc/anotherNonexistentFile'.
	proc _ self
				forkJob: '/bin/ls'
				arguments: args
				environment: nil
				descriptors: desc.
	proc ifNil: [self noAccessorAvailable].
	out close.
	err close.
	^ proc! !

!UnixProcess class methodsFor: 'X display - deprecated' stamp: 'dtl 8/5/2003 22:10'!
canConnectToXDisplay: xDisplayName
	"Deprecated. See ThisOSProcess>canConnectToXDisplay:"

	"self canConnectToXDisplay: ':0.0' "

	^ self thisOSProcess canConnectToXDisplay: xDisplayName
! !

!UnixProcess class methodsFor: 'X display - deprecated' stamp: 'dtl 8/5/2003 22:10'!
currentXDisplayName
	"Deprecated. See ThisOSProcess>>currentXDisplayName"

	"self currentXDisplayName"

	^ self thisOSProcess currentXDisplayName
! !

!UnixProcess class methodsFor: 'X display - deprecated' stamp: 'dtl 8/5/2003 22:09'!
decapitate
	"Deprecated. See ThisOSProcess>>decapitate"

	"self decapitate"

	^ OSProcess thisOSProcess decapitate! !

!UnixProcess class methodsFor: 'X display - deprecated' stamp: 'dtl 8/5/2003 22:18'!
displayOnXServer: xDisplayName
	"Deprecated. See ThisOSProcess>>displayOnXServer"

	"self displayOnXServer: ':0.0' "
	"self displayOnXServer: 'unix:0' "
	"self displayOnXServer: ':1' "
	"self displayOnXServer: 'noSuchMachine'"
	"self displayOnXServer: 'noSuchMachine:0'" "<-make sure network is running first!!"

	^ OSProcess thisOSProcess displayOnXServer: xDisplayName! !

!UnixProcess class methodsFor: 'X display - deprecated' stamp: 'dtl 8/5/2003 22:15'!
recapitate
	"Deprecated. See ThisOSProcess>>recapitate"

	"self decapitate. (Delay forSeconds: 5) wait. self recapitate."

	^ OSProcess thisOSProcess recapitate! !

!UnixProcess class methodsFor: 'X display - deprecated' stamp: 'dtl 8/5/2003 22:18'!
setXDisplayName: xDisplayName
	"Deprecated. See ThisOSProcess>>setXDisplayName"

	"self setXDisplayName: 'unix:0' "
	"self setXDisplayName: ':1' "

	^ OSProcess thisOSProcess setXDisplayName: xDisplayName

! !

!UnixProcess class methodsFor: 'child process creation' stamp: 'dtl 12/27/2000 16:57'!
forkHeadlessSqueakAndDo: aBlock 
	"Start a new instance of Squeak running in a child OS process, and  
	execute aBlock in the child instance. The new instance is a clone of 
	this image, but without a connection to the X display. The child instance 
	executes aBlock, which hopefully does not involve interaction with the 
	X display; and the parent continues normally.  
	  
	The child should not depend on using existing connections to external  
	resources. For example, the child may lose its connections to stdin, 
	stdout, and stderr after its parent exits."

	"UnixProcess forkHeadlessSqueakAndDo: [UnixProcess helloWorld]"

	^ self thisOSProcess forkHeadlessSqueakAndDo: aBlock

! !

!UnixProcess class methodsFor: 'child process creation' stamp: 'dtl 12/27/2000 17:00'!
forkHeadlessSqueakAndDoThenQuit: aBlock 
	"Start a new instance of Squeak running in a child OS process, and 
	execute aBlock in the child instance. The new instance is a clone of
	this image, but without a connection to the X display. The child instance
	executes aBlock, which hopefully does not involve interaction with the
	X display; and the parent continues normally.

	The child should not depend on using existing connections to external 
	resources. For example, the child may lose its connections to stdin, stdout,
	and stderr after its parent exits."

	"UnixProcess forkHeadlessSqueakAndDoThenQuit: [UnixProcess helloWorld]"

	^ self thisOSProcess forkHeadlessSqueakAndDoThenQuit: aBlock
! !

!UnixProcess class methodsFor: 'child process creation' stamp: 'dtl 12/27/2000 16:48'!
forkJob: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams 
	"Call Unix vfork() and execve() to create a child process, and answer the 
	child process. Delegate this to the singleton OSProcess>>thisOSProcess."

	^ self thisOSProcess
		forkJob: executableFile
		arguments: arrayOfStrings
		environment: stringDictionary
		descriptors: arrayOf3Streams! !

!UnixProcess class methodsFor: 'child process creation' stamp: 'dtl 7/5/2000 07:19'!
forkSqueak
	"Start a new instance of Squeak running in a child OS process. The new instance is a
	clone of this image except for the return value of this method. It does not reload the
	image file from disk.

	The child should not depend on using existing connections to external resources. For
	example, the child may lose its connections to stdin, stdout, and stderr after its parent
	exits."

	"UnixProcess forkSqueak"

	^ self thisOSProcess forkSqueak.
! !

!UnixProcess class methodsFor: 'child process creation' stamp: 'dtl 10/8/2001 20:40'!
forkSqueakAndDo: aBlock
	"Start a new instance of Squeak running in a child OS process. The new instance is a
	clone of this image except for the return value of this method. It does not reload the
	image file from disk. The child image evaluates aBlock.

	The child should not depend on using existing connections to external resources. For
	example, the child may lose its connections to stdin, stdout, and stderr after its parent
	exits."

	"UnixProcess forkSqueakAndDo:
		[Object inform: 'Hi, I am the child Squeak process.']"

	^ self thisOSProcess forkSqueakAndDo: aBlock
! !

!UnixProcess class methodsFor: 'child process creation' stamp: 'dtl 10/8/2001 20:41'!
forkSqueakAndDoThenQuit: aBlock
	"Start a new instance of Squeak running in a child OS process. The new instance is a
	clone of this image except for the return value of this method. It does not reload the
	image file from disk. The child image evaluates aBlock.

	The child should not depend on using existing connections to external resources. For
	example, the child may lose its connections to stdin, stdout, and stderr after its parent
	exits."

	"UnixProcess forkSqueakAndDoThenQuit:
		[Object inform: 'Hi, I am the child Squeak process. Click OK to exit the child Squeak.']"

	^ self thisOSProcess forkSqueakAndDoThenQuit: aBlock
! !

!UnixProcess class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:49'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for
	the Squeak VM running on the current platform."

	^ self isUnix
! !

!UnixProcess class methodsFor: 'utility - VM building' stamp: 'dtl 10/19/2001 13:31'!
makeVmIn: buildDirectoryPathName
	"Rebuild the virtual machine and plugins in the buildDirectoryPathName
	directory. If the build is successful, save the image and restart using the
	new VM. This assumes that the currently executing VM is either located in,
	or linked to, the buildDirectoryPathName directory."

	"UnixProcess makeVmIn: '/usr/local/squeak/Squeak-2.8/build'"
	"UnixProcess makeVmIn: nil"

	^ self makeVmIn: (buildDirectoryPathName ifNil: ['']) interactive: true

! !

!UnixProcess class methodsFor: 'utility - VM building' stamp: 'dtl 10/19/2001 11:41'!
makeVmIn: buildDirectoryPathName interactive: aBoolean
	"Rebuild the virtual machine and plugins in the buildDirectoryPathName
	directory. If the build is successful, save the image and restart using the
	new VM. This assumes that the currently executing VM is either located in,
	or linked to, the buildDirectoryPathName directory. If aBoolean is true,
	display interactive dialogs, otherwise output goes only to the Transcript."

	"UnixProcess makeVmIn: '/usr/local/squeak/Squeak-2.8/build' interactive: true"

	| commandString result |
	commandString _ 'cd ', buildDirectoryPathName, '; make'.
	Transcript cr; show: 'running external command "', commandString, '"'; cr.
	result _ self waitForCommandOutputArray: commandString.
	((result at: 3) == 0 and: [(result at: 2) isEmpty])
		ifTrue:
			[Transcript show: 'make completed successfully, restarting VM'; cr.
			aBoolean ifTrue:
				[self inform: 'make completed successfully, restarting VM'].
			self quitAndRestart]
		ifFalse:
			[Transcript show: (result at: 2); cr.
			Transcript show: 'make did not succeed, VM will not be restarted'; cr.
			aBoolean ifTrue:
				[self inform: 'make did not succeed, VM will not be restarted'.
				self inform: (result at: 2)]].
	^ result
! !

!UnixProcess class methodsFor: 'unit tests' stamp: 'dtl 3/5/2005 14:19'!
runTests
	"Run a few tests to see if things are working correctly on Unix/Linux. 
	Output is on stdout, stderr, and the Squeak Transcript. One of the tests 
	requires input from stdin, so Squeak should be run from a shell command
	line and not as a background process.

	Warning: This test will crash your VM if your are using the -xshm command
	line option. For reasons which I do not quite understand, the X shared
	memory segment becomes invalid when the Squeak VM which initially
	opened the shared memory exits. The remaining Squeak children will crash
	when then then next try to update the display.

	Note: If you see 'select: Bad file descriptor' messages on your console standard
	output, these are occuring while running headless in the decapitate/recapitate
	tests.

	Important: Prior to evaluating this method, please type one line of text
	followed by a <cr> on the terminal standard input. This provides the
	input for the stdin test. Failing to provide this input prior to evaluating
	the tests will cause one of the test cases to fail."

	"UnixProcess runTests"

	| this s p failures result a |
	failures _ 0.
	Transcript show: 'Begin OSProcess tests'; cr.
	Transcript show: 'Test for working ProcessAccessor ... '.
	this _ OSProcess thisOSProcess.
	(this pid isKindOf: Integer)
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'Echo one line of text previously entered from stdin ... '.
	s _ OSProcess readFromStdIn.
	s size > 0
		ifTrue: [Transcript show: 'OK'; cr; show: s; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'Message to stdout ... '.
	(OSProcess helloWorld isKindOf: AttachableFileStream)
		ifTrue: [Transcript show: 'OK'; cr; show: s; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'Message to stderr ... '.
	(OSProcess helloStdErr isKindOf: AttachableFileStream)
		ifTrue: [Transcript show: 'OK'; cr; show: s; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess cataFile ... '.
	p _ UnixProcess catAFile.
	(Delay forSeconds: 1) wait.
	p exitStatus == 0
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess testEnvSet ... '.
	p _ UnixProcess testEnvSet.
	(Delay forSeconds: 1) wait.
	p exitStatus == 0
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess catFromFileToFiles ... '.
	OSProcess command: 'rm /tmp/deleteMe.out'.
	OSProcess command: 'rm /tmp/deleteMe.err'.
	p _ UnixProcess catFromFileToFiles.
	(Delay forSeconds: 1) wait.
	p exitStatus == 0
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess testRunCommand ... '.
	OSProcess command: 'rm /tmp/deleteMe.out'.
	OSProcess command: 'rm /tmp/deleteMe.err'.
	p _ UnixProcess testRunCommand.
	(Delay forSeconds: 1) wait.
	p exitStatus == 256
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess testPipe ... '.
	UnixProcess testPipe = 'this is some text to write into the pipe'
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess testPipeLine ... '.
	UnixProcess testPipeLine = 'This is the text to write out through one pipe, copy through an external cat command, and then read back in through another pipe.'
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess spawnTenHeadlessChildren ... '.
	p _ UnixProcess spawnTenHeadlessChildren.
	(p size == 10 and: [(p select: [:e | (e runState == #running) | (e exitStatus == 0)]) size == 10])
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess cloneSqueak ... '.
	p _ UnixProcess cloneSqueak.
	(p isKindOf: UnixProcess)
		ifTrue: 
			[(Delay forSeconds: 1) wait.
			OSProcess snapshot: false andQuit: true].
	(Delay forSeconds: 5) wait.
	p exitStatus == 0
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess squeakSqueak ... '.
	p _ UnixProcess squeakSqueak.
	(Delay forSeconds: 5) wait.
	OSProcess thisOSProcess sigkill: p.
	(Delay forSeconds: 1) wait.
	p exitStatus == 9
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess restartVirtualMachine ... '.
	p _ UnixProcess restartVirtualMachine.
	p pid == OSProcess thisOSProcess pid
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess command: ''cat'' input: ''this is some test data'' ... '.
	p _ (OSProcess thisOSProcess
		command: 'cat'
		input: 'this is some test data').
	(Delay forSeconds: 1) wait.
	p upToEnd = 'this is some test data'
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'UnixProcess waitForCommandOutput: ''echo sleeping...; sleep 1; echo I just slept for one second'' ... '.
	('*I just slept for one second*' match:
		(OSProcess thisOSProcess
			waitForCommandOutput: 'echo sleeping...; sleep 1; echo I just slept for one second'))
		ifTrue: [Transcript show: 'OK'; cr]
		ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1].
	Transcript show: 'Unix command pipeline with output and error returned in an array ... '.
	(Smalltalk hasClassNamed: #PipeableOSProcess)
		ifTrue:
			[a _ (((Smalltalk at: #PipeableOSProcess) command: 'echo this is a test; BOGUS') | 'cut -c11-14') outputAndError.
			(((a isKindOf: Array)
				and: ['test*' match: (a at: 1)])
					and: ['*BOGUS*' match: (a at: 2)])
						ifTrue: [Transcript show: 'OK'; cr]
						ifFalse: [Transcript show: 'NFG'; cr. failures _ failures + 1]]
		ifFalse:
			[Transcript show: 'skipping PipeableOSProcess test (requires CommandShell)'; cr].

	Transcript show: 'UnixProcess decapitate and recapatiate five times'; cr.
	5 timesRepeat:
		[UnixProcess decapitate.
		UnixProcess recapitate].

	failures == 1
		ifTrue: [result _ 'OSProcess tests completed with ', failures printString, ' failure']
		ifFalse: [result _ 'OSProcess tests completed with ', failures printString, ' failures'].
	Transcript show: result; cr.

	^ result
! !

!UnixProcess class methodsFor: 'utility - image save' stamp: 'dtl 7/23/2010 07:50'!
saveImageInBackground
	"Save image in a background OS process."

	"UnixProcess saveImageInBackground"

	^ self saveImageInBackground: self uniqueNameForSavedImage nice: false
! !

!UnixProcess class methodsFor: 'utility - image save' stamp: 'dtl 1/20/2018 20:24:18'!
saveImageInBackground: savedImageName nice: niceFlag
	"When Squeak is used as a server it is sometimes desirable to periodically
	save image snapshots. This method forks a headless Squeak to perform a
	snapshot without impacting the server Squeak. Very little additional memory
	is required to do this because Unix copy-on-write memory management allows
	the two Squeak images to share object memory while the save is performed.
	The saved image is given a time stamped name, and the image name of
	the main server Squeak remains unchanged. If niceFlag is true, the
	background OS process runs at lowered scheduling priority."

	^ self forkHeadlessSqueakAndDo:
		[| st |
		"Delay is required in the background process when forking a headless
		Squeak, probably necessary to permit X session stuff to settle down."
		(Delay forMilliseconds: 500) wait.
		(niceFlag notNil and: [niceFlag]) ifTrue:
			["lower priority of background OS process"
			OSProcess accessor nice: 1].
		st _ SmalltalkImage current.
		(SourceFiles at: 2) ifNotNil:
			["ensure that copying the changes file will always work"
			st closeSourceFiles.
			OSProcess saveChangesInFileNamed: (st fullNameForChangesNamed: savedImageName)].
		st changeImageNameTo: savedImageName,'.image';
			closeSourceFiles;
			openSourceFiles;  "so SNAPSHOT appears in new changes file"
			saveImageSegments;
			snapshot: true andQuit: true]
! !

!UnixProcess class methodsFor: 'utility - image save' stamp: 'dtl 7/23/2010 07:50'!
saveImageInBackgroundNicely
	"Save image in a background OS process with lowered scheduling priority."

	"UnixProcess saveImageInBackgroundNicely"

	^ self saveImageInBackground: self uniqueNameForSavedImage nice: true
! !

!UnixProcess class methodsFor: 'utility - image save' stamp: 'dtl 11/8/2005 11:39'!
uniqueNameForSavedImage
	"A time stamped image name that will sort in date order in a directory listing"

	"UnixProcess uniqueNameForSavedImage"

	| now month day hour minute second |
	now _ DateAndTime now.
	month _ now month asString.
	day _ now dayOfMonth asString.
	hour _ now hour24 asString.
	minute _ now minute asString.
	second _ now second asString.
	^ 'squeak-',
		now year asString,
		(month size < 2 ifTrue: ['0', month] ifFalse: [month]),
		(day size < 2 ifTrue: ['0', day] ifFalse: [day]),
		(hour size < 2 ifTrue: ['0', hour] ifFalse: [hour]),
		(minute size < 2 ifTrue: ['0', minute] ifFalse: [minute]),
		(second size < 2 ifTrue: ['0', second] ifFalse: [second])
! !

!UnixProcess class methodsFor: 'external command processing' stamp: 'dtl 1/18/2001 23:14'!
waitForCommandOutput: aCommandString 
	"Run a command in a shell process. Similar to the system(3) call in the 
	standard C library. The active Smalltalk process waits for completion of
	the external command process."

	"UnixProcess waitForCommandOutput: 'echo sleeping...; sleep 1; echo I just slept for one second'"
	"UnixProcess waitForCommandOutput: 'ThisIsABogusCommand'"

	^ self thisOSProcess waitForCommandOutput: aCommandString
! !

!UnixProcess class methodsFor: 'external command processing' stamp: 'dtl 1/18/2001 23:14'!
waitForCommandOutputArray: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in the standard C library.
	The active Smalltalk process waits for completion of the external command process."

	"UnixProcess waitForCommandOutputArray: 'echo Hello world!!; ls /NOSUCHFILE'"

	^ self thisOSProcess waitForCommandOutputArray: aCommandString! !

!WindowsProcess methodsFor: 'accessing' stamp: 'dtl 2/22/2002 22:02'!
environment

	^ environment
! !

!WindowsProcess methodsFor: 'accessing' stamp: 'dtl 2/28/2002 07:15'!
mainThread
	"The main thread for this OS process. The handle for this thread is a
	pseudo-handle, and cannot be used to close the main thread."

	^ mainThread ifNil: [mainThread _ processAccessor getMainThread]! !

!WindowsProcess methodsFor: 'accessing' stamp: 'dtl 2/22/2002 16:43'!
pid

	^ pid _ self processAccessor primGetPid
! !

!WindowsProcess methodsFor: 'accessing' stamp: 'dtl 2/28/2002 07:26'!
processHandle
	"The handle for this OS process. This is a pseudo-handle, a constant provided
	by Windows to represent the process. Note that the main thread handle is also
	represented by a pseudo-handle."

	^ processHandle ifNil: [processHandle _ self processAccessor primGetPidHandle]
! !

!WindowsProcess methodsFor: 'accessing' stamp: 'dtl 2/28/2002 08:04'!
threads
	"One or more threads of execution within the OS process. The main
	thread for the process is held by the mainThread variable and is not
	included in this collection. Threads are created to wait for the exit of
	child processes, so this collection grows as child processes are created."

	^ threads ifNil: [threads _ OrderedCollection new]
! !

!WindowsProcess methodsFor: 'child process management' stamp: 'dtl 2/26/2002 16:08'!
activeHandles
	"Answer an Array of handles for all children that are believed to be running."

	^ (self activeChildren collect: [:c | c handle]) asArray
! !

!WindowsProcess methodsFor: 'child process management' stamp: 'dtl 2/5/2015 20:06'!
registerChildProcess: anOSProcess
	"Register the external process and set an exit handler thread to signal when
	the process exits."

	super registerChildProcess: anOSProcess.
	self threads add: self restartChildWatcherThread.
	^ anOSProcess
! !

!WindowsProcess methodsFor: 'child process management' stamp: 'dtl 1/13/2007 09:49'!
restartChildWatcherThread
	"Set an exit handler thread to signal when the process exits."


	^ self processAccessor restartChildWatcherThread: self activeHandles
! !

!WindowsProcess methodsFor: 'child process management' stamp: 'dtl 2/28/2002 08:17'!
updateActiveChildren

	super updateActiveChildren.
	self restartChildWatcherThread
! !

!WindowsProcess methodsFor: 'console' stamp: 'dtl 9/7/2002 20:57'!
closeConsole
	"Close the console. The standard input, output and error streams will no longer be available."

	"OSProcess thisOSProcess closeConsole"

	self processAccessor primFreeConsole.
	self refreshFromProcessAccessor
! !

!WindowsProcess methodsFor: 'console' stamp: 'dtl 9/7/2002 20:57'!
openConsole
	"Open a console. This makes the standard input, output and error streams available."

	"OSProcess thisOSProcess openConsole"

	self processAccessor primAllocConsole.
	self refreshFromProcessAccessor
! !

!WindowsProcess methodsFor: 'child process creation' stamp: 'dtl 2/28/2002 14:48'!
command: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in
	the standard C library, except that aCommandString runs asynchronously
	in a child process. Answer an instance of ExternalWindowsProcess which
	is a proxy for the new Windows process."

	"OSProcess command: 'SOL'"

	^ ExternalWindowsOSProcess command: aCommandString
! !

!WindowsProcess methodsFor: 'child process creation' stamp: 'dtl 10/18/2001 20:19'!
forkAndExec: executableFile arguments: arrayOfStrings environment: stringDictionary descriptors: arrayOf3Streams
	"Use my processAccessor to call vfork() and execve() and create a
	new child task. Answer a proxy for the new task, an instance of
	ExternalWindowsProcess."

	self notYetImplemented
! !

!WindowsProcess methodsFor: 'child process creation' stamp: 'dtl 11/24/2008 17:47'!
waitForCommand: aCommandString
	"Run a command in a shell process. Similar to the system(3) call in the standard C library.
	The active Smalltalk process waits for completion of the external command process. This just
	uses a simple polling loop, which is not very elegant but works well enough for most purposes."

	| proc d |
	d _ Delay forMilliseconds: 50.
	proc _ self command: aCommandString.
	
	proc ifNil: [self class noAccessorAvailable].
	[proc runState == #complete] whileFalse: [d wait].
	^ proc
! !

!WindowsProcess methodsFor: 'environment' stamp: 'DamienCassou 5/3/2012 11:53'!
environmentAt: aSymbol
	"Answer an environment variable for the external OS process, and update the dictionary
	in this Smalltalk object."

	^ environment at: aSymbol asSymbol ifAbsent: []
! !

!WindowsProcess methodsFor: 'environment' stamp: 'DamienCassou 5/3/2012 11:53'!
environmentAt: aSymbol put: aString
	"Set an environment variable for the external OS process, and update the dictionary in
	this Smalltalk object."

	| s |
	self initialize.
	s _ self processAccessor environmentAt: aSymbol put: aString.
	s ifNotNil: [ self environment at: aSymbol asSymbol put: aString ].
	^ s

! !

!WindowsProcess methodsFor: 'environment' stamp: 'dtl 1/24/2013 19:23'!
getCwd
	"Get current working directory. If this cannot be obtained from the
	environment, answer a reasonable default."

	"OSProcess thisOSProcess getCwd"

	^ self processAccessor primGetCurrentWorkingDirectory
		ifNil: [OSProcess defaultPathString]


! !

!WindowsProcess methodsFor: 'environment' stamp: 'dtl 9/25/2005 05:11'!
path
	"Newer versions of Windows mixed case"

	^ self environment
		at: #PATH
		ifAbsent: [environment
				at: #Path
				ifAbsent: ['']]! !

!WindowsProcess methodsFor: 'private' stamp: 'dtl 2/22/2002 22:00'!
getEnvironmentDictionary
	"Answer an environment dictionary using an OSProcessAccessor."

	"OSProcess thisOSProcess getEnvironmentDictionary"

	| strings env |
	strings _ processAccessor primGetEnvironmentStrings.
	strings isNil
		ifTrue:
			[^ nil]
		ifFalse:
			[env _ Dictionary new.
			strings do: [:s |
				env at: (s copyUpTo: $=) asSymbol put: (s copyAfterLast: $=)].
			^ env]
! !

!WindowsProcess methodsFor: 'private' stamp: 'dtl 9/7/2002 20:55'!
setStdErr
	"Reset to nil if the console has been closed"

	^ self processAccessor getStdErr isNil
		ifTrue: [stdErr _ nil]
		ifFalse: [super setStdErr]
! !

!WindowsProcess methodsFor: 'private' stamp: 'dtl 9/7/2002 20:55'!
setStdIn
	"Reset to nil if the console has been closed"

	^ self processAccessor getStdIn isNil
		ifTrue: [stdIn _ nil]
		ifFalse: [super setStdIn]
! !

!WindowsProcess methodsFor: 'private' stamp: 'dtl 9/7/2002 20:55'!
setStdOut
	"Reset to nil if the console has been closed"

	^ self processAccessor getStdOut isNil
		ifTrue: [stdOut _ nil]
		ifFalse: [super setStdOut]
! !

!WindowsProcess methodsFor: 'initialize - release' stamp: 'dtl 7/9/2021 18:36'!
initialize
	"Set my instance variables to reflect the state of the OS process in which 
	this Smalltalk virtual machine is executing. On Windows, we cannot rely
	on the pid to have changed when the VM is restarted, so use a one-shot
	function to determine if we are restarting the VM (as opposed to returning
	from an image save)."

	super initialize.
	(self processAccessor canAccessSystem not or: [processAccessor primOneShot]) ifTrue:
		["Restarting the VM in a new process"
		OSProcessAccessor initialize. "new instance required in mingw environment"
		self resetThreads.
		self threads; mainThread.
		processHandle _ nil.
		self processHandle].
	self refreshFromProcessAccessor
! !

!WindowsProcess methodsFor: 'initialize - release' stamp: 'dtl 2/28/2002 07:16'!
resetThreads
	"If any cleanup is required, do it here."

	threads _ nil.
	mainThread _ nil
! !

!WindowsProcess methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:50'!
isResponsibleForThisPlatform
	"Answer true is this is an instance of the class which is responsible for representing
	the OS process for the Squeak VM running on the current platform. A false answer is
	usually the result of running the image on a different platform and VM."

	^ self class isWindows
! !

!WindowsProcess methodsFor: 'updating' stamp: 'dtl 9/7/2002 20:47'!
refreshFromProcessAccessor
	"Set my instance variables to reflect the state of the OS process in which this Smalltalk
	virtual machine is executing."

	sessionID _ self processAccessor getSessionIdentifier.
	pid _ processAccessor primGetPid.
	self setStdIn.
	self setStdOut.
	self setStdErr.
	environment _ self getEnvironmentDictionary
! !

!WindowsProcess methodsFor: 'updating' stamp: 'dtl 2/25/2002 08:29'!
update: aParameter
	"Framework to update some or all of the instance variables based on external events,
	such as receipt of a sigchd signal when a child process exits."

	(aParameter == (self processAccessor)) ifTrue:
		[^ self refreshFromProcessAccessor; yourself].
	(aParameter == #pid) ifTrue:
		[^ self resetChildProcessDictionary. "Forget children of prior process"].
	(aParameter == #childProcessStatus) ifTrue:
		[^ self updateActiveChildren; changed; yourself].
	(aParameter == #startUp) ifTrue:
		[^ self update: #pid].
	aParameter == #invalidProcessAccessor ifTrue:
		[processAccessor _ processAccessor ifNotNil: [processAccessor removeDependent: self. nil].
		^ self].
	self error: 'Unexpected update parameter'! !

!WindowsProcess class methodsFor: 'platform identification' stamp: 'dtl 8/30/2003 17:50'!
isResponsibleForThisPlatform
	"Answer true if this class is responsible for representing the OS process for
	the Squeak VM running on the current platform."

	^ self isWindows
! !

!UnixProcessExitStatus methodsFor: 'printing' stamp: 'dtl 8/22/2012 23:18'!
printOn: aStream

	self isExited
		ifTrue: [aStream nextPutAll: 'normal termination with status ';
				 nextPutAll: self statusIfExited asString].
	self isSignaled
		ifTrue: [aStream nextPutAll: 'exit due to signal ';
				nextPutAll: self statusIfSignaled asString].
	self isStopped
		ifTrue: [aStream nextPutAll: 'stopped due to signal ';
				nextPutAll: self statusIfStopped].! !

!UnixProcessExitStatus methodsFor: 'accessing' stamp: 'dtl 8/22/2012 23:10'!
exitStatus
	self isExited ifTrue: [^self statusIfExited].
	self isSignaled ifTrue: [^self statusIfSignaled].
	self isStopped ifTrue: [^self statusIfStopped].
	self notify: 'cannot decode exit status ', intValue asString.
	^intValue! !

!UnixProcessExitStatus methodsFor: 'accessing' stamp: 'dtl 8/22/2012 23:11'!
statusIfExited
	"
	/* If WIFEXITED(STATUS), the low-order 8 bits of the status.  */
	#define __WEXITSTATUS(status)   (((status) & 0xff00) >> 8)
	"
	^(intValue bitAnd: 16rFF00) >> 8! !

!UnixProcessExitStatus methodsFor: 'accessing' stamp: 'dtl 8/22/2012 23:11'!
statusIfSignaled
	"
	/* If WIFSIGNALED(STATUS), the terminating signal.  */
	#define __WTERMSIG(status)      ((status) & 0x7f)
	"
	^intValue bitAnd: 16r7F! !

!UnixProcessExitStatus methodsFor: 'accessing' stamp: 'dtl 8/22/2012 22:28'!
statusIfStopped
	"
	/* If WIFSTOPPED(STATUS), the signal that stopped the child.  */
	#define __WSTOPSIG(status)      __WEXITSTATUS(status)
	"
	^self statusIfExited! !

!UnixProcessExitStatus methodsFor: 'accessing' stamp: 'dtl 8/22/2012 23:11'!
value
	^intValue! !

!UnixProcessExitStatus methodsFor: 'initialize-release' stamp: 'dtl 8/22/2012 23:10'!
for: anInteger
	intValue _ anInteger! !

!UnixProcessExitStatus methodsFor: 'testing' stamp: 'dtl 8/22/2012 22:32'!
isExited
	"
	/* Nonzero if STATUS indicates normal termination.  */
	#define __WIFEXITED(status)     (__WTERMSIG(status) == 0)
	"
	^ self statusIfSignaled = 0

! !

!UnixProcessExitStatus methodsFor: 'testing' stamp: 'dtl 8/22/2012 23:11'!
isSignaled
	"
	/* Nonzero if STATUS indicates termination by a signal.  */
	#define __WIFSIGNALED(status) \
	  (((signed char) (((status) & 0x7f) + 1) >> 1) > 0)
	"
	^(((intValue bitAnd: 16r7F) + 1) >> 1) > 0

! !

!UnixProcessExitStatus methodsFor: 'testing' stamp: 'dtl 8/22/2012 23:11'!
isStopped
	"
	/* Nonzero if STATUS indicates the child is stopped.  */
	#define __WIFSTOPPED(status)    (((status) & 0xff) == 0x7f)
	"
	^(intValue bitAnd: 16rFF) = 16r7F

! !

!UnixProcessExitStatus methodsFor: 'testing' stamp: 'dtl 8/22/2012 23:31'!
isSuccess
	^self exitStatus = 0! !

!UnixProcessExitStatus class methodsFor: 'instance creation' stamp: 'dtl 8/22/2012 21:24'!
for: integerValue

	^self new for: integerValue
! !

!WindowsThread methodsFor: 'printing' stamp: 'dtl 2/27/2002 12:02'!
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ' (', self threadID printString, ', ', self runState, ')'
! !

!WindowsThread methodsFor: 'initialize - release' stamp: 'dtl 2/25/2002 07:38'!
closeHandle
	"Clean up after thread exits."

	OSProcess accessor primCloseHandle: handle.
	handle _ nil
! !

!WindowsThread methodsFor: 'initialize - release' stamp: 'dtl 2/25/2002 07:03'!
initialize

	self runState! !

!WindowsThread methodsFor: 'initialize - release' stamp: 'dtl 2/27/2002 11:35'!
terminate
	"Force an exit. No cleanup is performed. Use with caution for a thread which
	is (for example) manipulating a mutex."

	(self isRunning and: [self handle notNil]) ifTrue:
		[OSProcess accessor primTerminateThread: self handle.
		self complete]
! !

!WindowsThread methodsFor: 'setting run state' stamp: 'dtl 2/26/2002 10:45'!
complete
	"Thread has exited."

	self closeHandle; runState: #complete
! !

!WindowsThread methodsFor: 'setting run state' stamp: 'dtl 2/25/2002 06:36'!
running
	"Thread is scheduled to run."

	self runState: #running
! !

!WindowsThread methodsFor: 'setting run state' stamp: 'dtl 2/25/2002 06:36'!
unknownRunState
	"Unable to determine the current run state of the thread, possibly because
	this is a stale reference to a thread which no longer exists."

	self runState: #unknownRunState
! !

!WindowsThread methodsFor: 'accessing' stamp: 'dtl 2/25/2002 06:39'!
handle
	"A Windows HANDLE represented as a ByteArray."

	^ handle! !

!WindowsThread methodsFor: 'accessing' stamp: 'dtl 2/25/2002 06:38'!
handle: aHandleObject
	"A Windows HANDLE represented as a ByteArray."

	handle _ aHandleObject! !

!WindowsThread methodsFor: 'accessing' stamp: 'dtl 1/25/2004 11:01'!
runState

	^ runState ifNil: [self unknownRunState]
! !

!WindowsThread methodsFor: 'accessing' stamp: 'dtl 2/25/2002 07:02'!
runState: aSymbol

	runState _ aSymbol
! !

!WindowsThread methodsFor: 'accessing' stamp: 'dtl 2/25/2002 06:40'!
threadID
	"A unique identifier for the thread."

	^ threadID
! !

!WindowsThread methodsFor: 'accessing' stamp: 'dtl 2/25/2002 06:40'!
threadID: anInteger
	"A unique identifier for the thread."

	threadID _ anInteger
! !

!WindowsThread methodsFor: 'testing' stamp: 'dtl 2/25/2002 06:37'!
isComplete

	^ self runState == #complete! !

!WindowsThread methodsFor: 'testing' stamp: 'dtl 2/25/2002 06:37'!
isRunning

	^ self runState == #running! !

!WindowsThread class methodsFor: 'instance creation' stamp: 'dtl 2/25/2002 07:00'!
threadID: anInteger handle: aHandleObject

	^ super new threadID: anInteger; handle: aHandleObject; initialize
! !

!WindowsThread class methodsFor: 'instance creation' stamp: 'dtl 2/26/2002 07:17'!
threadID: anInteger handle: aHandleObject running: trueOrFalse

	| thread |
	thread _ super new threadID: anInteger; handle: aHandleObject; initialize.
	trueOrFalse ifTrue: [thread running].
	^ thread
! !

!AioEventHandlerExample methodsFor: 'initialize-release' stamp: 'dtl 9/4/2003 06:20'!
close

	self handler close.
	self handler removeDependent: self
! !

!AioEventHandlerExample methodsFor: 'updating' stamp: 'dtl 2/24/2013 10:22'!
getAvailableData
	"Obtain all available data from ioStream. For a FileStream, keep reading until
	a line terminator is reached. This allows use with a FileStream that has not been
	set for nonblocking input."

	| ws c buffer n |
	buffer _ String new: 4000.
	(self ioStream isKindOf: FileStream)
		ifTrue:
			[ws _ WriteStream on: String new.
			[c _ ioStream next.
			(c == Character lf)
				ifTrue: [ws nextPut: Character cr]
				ifFalse: [ws nextPut: c].
			(c ~= Character lf) and: [c ~= Character cr]] whileTrue.
			^ ws contents]
		ifFalse:
			[ioStream dataAvailable
				ifTrue:
					[n _ ioStream receiveDataInto: buffer.
					^ buffer copyFrom: 1 to: n]
				ifFalse:
					[^ '']]
! !

!AioEventHandlerExample methodsFor: 'updating' stamp: 'dtl 7/5/2003 18:23'!
update: anObject

	(anObject isKindOf: AioEventHandler)
		ifTrue: [Transcript show: self getAvailableData]
! !

!AioEventHandlerExample methodsFor: 'accessing' stamp: 'dtl 7/5/2003 09:40'!
handler

	^ handler! !

!AioEventHandlerExample methodsFor: 'accessing' stamp: 'dtl 7/5/2003 09:40'!
handler: anAioHandler

	handler _ anAioHandler! !

!AioEventHandlerExample methodsFor: 'accessing' stamp: 'dtl 7/5/2003 09:57'!
ioStream

	^ ioStream! !

!AioEventHandlerExample methodsFor: 'accessing' stamp: 'dtl 7/5/2003 09:57'!
ioStream: aFileStream

	ioStream _ aFileStream! !

!AioEventHandlerExample class methodsFor: 'examples' stamp: 'dtl 9/4/2003 06:22'!
osPipeExample
	"Demonstrate an asynchronous read hander on an OS pipe. Output will
	be displayed on the Transcript."

	"self osPipeExample"

	| pipe aio |
	(Smalltalk hasClassNamed: #OSProcess)
		ifFalse:
			[self notify: 'this example requires OSProcess']
		ifTrue:
			[pipe _ (Smalltalk at: #OSPipe) new.
			aio _ super new handler: (AioEventHandler for: pipe reader); ioStream: pipe reader.
			aio handler addDependent: aio.
			(1 to: 10) do:
				[:i |
				pipe nextPutAll: 'this is line ', i asString; cr.
				(Delay forMilliseconds: 500) wait].
			^ aio handler close]
! !

!AioEventHandlerExample class methodsFor: 'examples' stamp: 'dtl 9/4/2003 06:22'!
showTtyOnTranscript: ttyName
	"Enter lines on /dev/tty, and watch them show up on the Transcript. Normally,
	/dev/tty corresponds to standard input for the Squeak process, so if you have
	a serial port connected to something that generates data, try this example with
	/dev/whatever to demonstrate asych input on a serial port.

	Warning: This method does not set the file stream for nonblocking input, so
	it can block the Squeak VM. The #getAvailableData attempts to work around this,
	but save your image before testing with another serial interfaces."

	"self showTtyOnTranscript: '/dev/tty'"

	| inputStream handler example |
	inputStream _ FileStream readOnlyFileNamed: '/dev/tty'.
	handler _ AioEventHandler for: inputStream
				exceptions: true
				readEvents: true
				writeEvents: false.
	example _ super new handler: handler; ioStream: inputStream.
	handler addDependent: example.
	Transcript cr; show: ''.
	self notify: 'Enter lines on ', ttyName, ', watch the Transcript, and select "Proceed" when done'.
	^ example handler close
! !

!AioEventHandlerExample class methodsFor: 'examples' stamp: 'dtl 9/4/2003 06:23'!
standardInputExample
	"Enter lines on stdin, and watch them show up on the Transcript."

	"self standardInputExample"

	| aio stdin |
	(Smalltalk hasClassNamed: #OSProcess)
		ifFalse:
			[self notify: 'this example requires OSProcess']
		ifTrue:
			[Transcript cr; show: ''.
			stdin _ (Smalltalk at: #OSProcess) thisOSProcess stdIn.
			aio _ super new handler: (AioEventHandler for: stdin); ioStream: stdin.
			aio handler addDependent: aio.
			self notify: 'Enter lines on standard input, watch the Transcript, and select "Proceed" when done'.
			^ aio handler close]
! !

!AioEventHandlerExample class methodsFor: 'examples' stamp: 'dtl 9/4/2003 06:24'!
tcpSocketExample
	"Loosely based on OldSocket>>remoteTestServerTCP. Output is displayed on the Transcript."

	"self tcpSocketExample"

	| port serverTcpSocket serverName clientTcpSocket handler example useOldStyleSockets |
	port _ 8086.
	serverName _ '127.0.0.1'.

	"The networking code was updated for Squeak 3.6. This checks for which version to use."
	useOldStyleSockets _ Socket respondsTo: #initializeNetworkIfFail:.

	Transcript show: 'initializing network ... '.

	useOldStyleSockets
		ifTrue:
			[Socket initializeNetworkIfFail: [^Transcript show:'failed']]
		ifFalse:
			[[Socket initializeNetwork]
				on: Error
				do: [:ex | ^Transcript show:'failed']].
	Transcript show:'ok';cr.

	"Create the server (reader) socket"
	serverTcpSocket _ Socket newTCP.
	serverTcpSocket listenOn: port.
	[Transcript show: 'server endpoint created on port ', port asString; cr.
	useOldStyleSockets
		ifTrue:
			[serverTcpSocket waitForConnectionUntil: Socket standardDeadline]
		ifFalse:
			[serverTcpSocket waitForConnectionFor: 10]] fork.
	(Delay forMilliseconds: 1000) wait.

	"Create the client (writer) socket"
	clientTcpSocket _ Socket newTCP.
	clientTcpSocket connectTo: (NetNameResolver addressFromString: serverName) port: port.
	useOldStyleSockets
		ifTrue:
			[clientTcpSocket waitForConnectionUntil: Socket standardDeadline]
		ifFalse:
			[clientTcpSocket waitForConnectionFor: 10].
	Transcript show: 'client endpoint connected to ', serverName, ' port ', port asString; cr.

	"Set up a read event handler on the server socket"
	handler _ AioEventHandler for: serverTcpSocket
				exceptions: true
				readEvents: true
				writeEvents: false.
	example _ super new handler: handler; ioStream: serverTcpSocket.
	handler addDependent: example.
	Transcript show: 'event handler started'; cr.

	"Send a few lines of data to the client socket, waiting briefly between lines.
	The event handler will watch the server socket, and copy data to the Transcript
	each time a new line of data is available to the server."
	(1 to: 10) do:
		[:i |
		clientTcpSocket sendData: 'this is line ', i asString, Character cr asString.
		(Delay forMilliseconds: 500) wait].

	clientTcpSocket closeAndDestroy.
	Transcript show: 'client endpoint closed'; cr.
	serverTcpSocket closeAndDestroy.
	Transcript show: 'server endpoint closed'; cr.
	example close.
	Transcript show: 'event handler stopped'; cr.
	^ Array with: example with: serverTcpSocket with: clientTcpSocket! !

!StandardFileStream methodsFor: '*OSProcess-Base' stamp: 'dtl 6/5/2006 06:59'!
atEndOfFile
	"Answer whether the receiver is at its end based on the result of
	the last read operation. This uses feof() to test the underlying file
	stream status, and can be used as an alternative to #atEnd, which
	does not properly report end of file status for an OSPipe."

	^ fileID isNil or: [OSProcess accessor isAtEndOfFile: fileID]
! !

!StandardFileStream methodsFor: '*OSProcess-Base' stamp: 'dtl 1/13/2007 16:44'!
fileID
	"The contents of fileID can and will change after calling this method.
	The sender should copy the result rather than depending on the result
	to be immutable."

	^ fileID! !

!StandardFileStream methodsFor: '*OSProcess-Base' stamp: 'dtl 6/5/2006 07:18'!
isPipe

	^ false
! !
OSProcessAccessor initialize!
AttachableFileStream initialize!
OSProcess initialize!
ThisOSProcess initialize!
