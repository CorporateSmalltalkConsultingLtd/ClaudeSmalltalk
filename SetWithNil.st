Object subclass: #SetElement
  instanceVariableNames: 
    ' wrappedObject '
  classVariableNames: ''
  poolDictionaries: '' !


!SetElement class methods !

comment

"SetElement wraps an object for storage in a SetWithNil. This allows nil to be stored
in hash-based collections that use nil as an empty-slot sentinel.

Most objects return self from asSetElement. UndefinedObject (nil) returns a
SetElement wrapping nil, so the Set can distinguish empty slots from stored nils.

See also: SetWithNil, Object>>asSetElement, UndefinedObject>>asSetElement."

    ! !


!SetElement methods !

= anObject 

	"Answer true if the receiver equals anObject. Delegates to the wrapped object."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    anObject class == self class
        ifTrue: [^wrappedObject = anObject wrappedObject]
        ifFalse: [^wrappedObject = anObject]!

enclosedElement 

	"Answer the wrapped object. Used by SetWithNil to unwrap elements."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^wrappedObject!

hash 

	"Answer the hash of the wrapped object for consistent hashing."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^wrappedObject hash!

printOn: aStream 

	"Print a representation of the wrapped object."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    aStream nextPutAll: 'a SetElement('.
    wrappedObject printOn: aStream.
    aStream nextPut: $)!

wrappedObject 

	"Answer the object wrapped by this SetElement."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^wrappedObject!

wrappedObject: anObject 

	"Set the object wrapped by this SetElement."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    wrappedObject := anObject!

asSetElement 

	"Wrap self in another SetElement so SetWithNil can store SetElement instances correctly.
	Without this override, a SetElement added to a SetWithNil would be unwrapped by
	enclosedElement on retrieval, returning the wrappedObject instead of the SetElement."
    #CodeTrackerBlock.  "Last Modified  18:15:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^SetElement new wrappedObject: self! !


Set subclass: #SetWithNil
  instanceVariableNames: 
    ''
  classVariableNames: ''
  poolDictionaries: '' !


!SetWithNil class methods !

comment

"SetWithNil is a Set that correctly handles nil as an element.

Standard Set uses nil as an empty-slot sentinel in its hash bucket array, so
add: nil is silently ignored, includes: nil always returns false, and adding nil
multiple times creates duplicates.

SetWithNil fixes this by wrapping nil in a SetElement via asSetElement before
storing it, and unwrapping via enclosedElement when retrieving. Non-nil objects
pass through unchanged (Object>>asSetElement returns self).

Usage:
  | s |
  s := SetWithNil new.
  s add: nil.
  s includes: nil.    true
  s add: nil.
  s size.             1

See also: SetElement, Object>>asSetElement, UndefinedObject>>asSetElement."

    ! !


!SetWithNil methods !

add: element 

	"Add element to the receiver. Wraps nil in a SetElement to avoid sentinel conflicts."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^(super add: element asSetElement) enclosedElement!

do: aBlock 

	"Evaluate aBlock for each element, unwrapping SetElements."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    super do: [:each | aBlock value: each enclosedElement]!

includes: element 

	"Answer true if the receiver contains element. Handles nil correctly."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^super includes: element asSetElement!

occurrencesOf: element 

	"Answer the number of occurrences of element. Handles nil correctly."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^super occurrencesOf: element asSetElement!

remove: element ifAbsent: aBlock 

	"Remove element from the receiver. Handles nil correctly."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^(super remove: element asSetElement ifAbsent: [^aBlock value]) enclosedElement! !


"Extension methods on Object and UndefinedObject"


!Object methods !

asSetElement 

	"Answer the receiver as a SetElement. Most objects return self."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^self!

enclosedElement 

	"Answer the receiver. Non-wrapped objects return self."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^self! !


!UndefinedObject methods !

asSetElement 

	"Answer a SetElement wrapping nil so Sets can store nil without sentinel conflicts."
    #CodeTrackerBlock.  "Last Modified  05:30:00 02/18/26"
    #LabWare. "Owner"
    #XBase. "Type"
    #V8. "Project"
    #V8. "Module"
    #None. "Unit"
    #JMM. "RevisedBy"
    #NotReviewed. "ReviewedBy"
    #JMMXXX. "LabTrackId"
    #CodeTrackerBlockEnd.  "Copyright 2026 LabWare Inc."

    ^SetElement new wrappedObject: self! !
