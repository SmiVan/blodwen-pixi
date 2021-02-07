module ESFFI
import Data.Maybe
import Data.Nat

%foreign "browser:lambda: (_, a) => console.debug(a)"
prim__consoleDebug : a -> PrimIO ()

debug : HasIO io => a -> io ()
debug = primIO . prim__consoleDebug

DEBUG : String -> a -> a
DEBUG msg a = unsafePerformIO (do
    debug msg
    pure a
    )

export
Cast Bool Double where
    cast True = 1.0
    cast False = 0.0

export
Cast Double Bool where
    cast 1.0 = True
    cast _ = False

data ArrayPtr : Type -> Type where [external]

export
data Array : (content : Type) -> Type where
    MkESArray : (store : ArrayPtr content) -> Array content

-- ESFFI.Array is full of hacks, a temporary measure until a better alternative is found.
namespace Array
    %foreign "javascript:lambda: (_, x) => BigInt(x.length)" -- Nat is represented as BigInt internally.
    prim__length : ArrayPtr ty -> PrimIO Nat

    -- This can never be less than zero so believe_me
    export
    length : HasIO io => Array content -> io Nat
    length (MkESArray a) = map believe_me $ primIO $ prim__length a

    %foreign "javascript:lambda: () => Array()"
    prim__new : PrimIO (ArrayPtr ty)

    export
    new : HasIO io =>  io (Array content)
    new = map MkESArray $ primIO $ prim__new
    
    -- UNSAFE! Assumes the content type is known to be correct.
    export
    ref : AnyPtr -> Array content
    ref ptr = MkESArray (believe_me ptr)

    -- It's a cheap hack - but it works.
    %foreign "javascript:lambda: (_, x, i) => (x[i] === undefined)?{h:0}:{h:1,a1:x[i]}"
    prim__readAt : ArrayPtr ty -> Nat -> PrimIO (Maybe ty)

    export
    readAt : HasIO io => Array content -> (index : Nat) -> io (Maybe content)
    readAt (MkESArray a) i = primIO $ prim__readAt a (believe_me i) 
    
    %foreign "javascript:lambda: (_, x, i, v) => x[i]=v"
    prim__writeAt : ArrayPtr ty -> Nat -> ty -> PrimIO ()

    export
    writeAt : HasIO io => Array content -> (index : Nat) -> content -> io ()
    writeAt (MkESArray a) i v= primIO $ prim__writeAt a (believe_me i) v

    export
    writeFromList : HasIO io => Array content -> (list : List content) -> {default Z cursor : Nat} -> io ()
    writeFromList _ Nil = pure ()
    writeFromList arr (item :: items) {cursor} = do
        writeAt arr cursor item
        writeFromList arr items {cursor = S cursor}

    export
    newFromList : HasIO io => List content -> io (Array content)
    newFromList list = do 
        arr <- new
        writeFromList arr list
        pure arr

    readingToList : HasIO io => (array : Array content) -> (cursor : Nat) -> {default Nil accumulator : List content} -> io (List content)
    readingToList arr cursor {accumulator} = do
        case !(readAt arr cursor) of
            Nothing   => pure accumulator -- TODO: Implement abort here, because this should be impossible to get to without live array edits?
            Just item => case cursor of
                Z       => pure $ item :: accumulator 
                S prev  => readingToList arr prev {accumulator= item :: accumulator } 

    export
    readToList : HasIO io => (array : Array content) -> io (List content)
    readToList arr = readingToList arr (pred !(length arr))

namespace Property

    bracketPropertyAccessor : (extraArgs: String) -> (suffix: String) -> String
    bracketPropertyAccessor extraArgs suffix = 
        "javascript:lambda: (p,n" ++ extraArgs ++ ") => p[n]" ++ suffix

    %foreign bracketPropertyAccessor "" ""
    export
    prim__access : AnyPtr -> String -> PrimIO AnyPtr

    public export
    data Boolean = ESBoo AnyPtr String
    
    namespace Boolean
        %foreign bracketPropertyAccessor "" ""
        prim__get : AnyPtr -> String -> PrimIO Double
        
        export
        get : HasIO io => Boolean -> io Bool
        get (ESBoo parent name) = map cast $ primIO $ prim__get parent name

        %foreign bracketPropertyAccessor ",x" "=x"
        prim__assign_set : AnyPtr -> String -> Double -> PrimIO ()

        modifyBool : HasIO io 
                => (prim__assign_func : AnyPtr -> String -> Double -> PrimIO ()) 
                -> Boolean 
                -> Bool 
                -> io ()
        modifyBool prim__assign_func (ESBoo parent name) x = 
            primIO $ prim__assign_func parent name (cast x)

        infixl 8 ::=, &&=, ||=
        export
        (::=) : HasIO io => Boolean -> Bool -> io ()
        (::=) = modifyBool prim__assign_set

    public export
    data Number = ESNum AnyPtr String

    namespace Number
        export
        %foreign bracketPropertyAccessor "" ""
        prim__get : AnyPtr -> String -> PrimIO Double

        %foreign bracketPropertyAccessor ",x" "=x"
        prim__assign_set : AnyPtr -> String -> Double -> PrimIO ()
        %foreign bracketPropertyAccessor ",x" "+=x"
        prim__assign_add : AnyPtr -> String -> Double -> PrimIO ()
        %foreign bracketPropertyAccessor ",x" "-=x"
        prim__assign_sub : AnyPtr -> String -> Double -> PrimIO ()
        %foreign bracketPropertyAccessor ",x" "*=x"
        prim__assign_mul : AnyPtr -> String -> Double -> PrimIO ()
        %foreign bracketPropertyAccessor ",x" "/=x"
        prim__assign_div : AnyPtr -> String -> Double -> PrimIO ()
        %foreign bracketPropertyAccessor ",x" "%=x"
        prim__assign_mod : AnyPtr -> String -> Double -> PrimIO ()
        %foreign bracketPropertyAccessor ",x" "**=x"
        prim__assign_pow : AnyPtr -> String -> Double -> PrimIO ()

        export
        get : HasIO io => Number -> io Double
        get (ESNum parent name) = primIO $ prim__get parent name

        modifyNum : HasIO io 
                => (prim__assign_func : AnyPtr -> String -> Double -> PrimIO ()) 
                -> Number 
                -> Double 
                -> io ()
        modifyNum prim__assign_func (ESNum parent name) x = 
            primIO $ prim__assign_func parent name x

        infixl 8 ::=, +=, -=, *=, /=, %=, **=
        export
        (::=) : HasIO io => Number -> Double -> io ()
        (::=) = modifyNum prim__assign_set
        export
        (+=) : HasIO io => Number -> Double -> io ()
        (+=) = modifyNum prim__assign_add
        export
        (-=) : HasIO io => Number -> Double -> io ()
        (-=) = modifyNum prim__assign_sub
        export
        (*=) : HasIO io => Number -> Double -> io ()
        (*=) = modifyNum prim__assign_mul
        export
        (/=) : HasIO io => Number -> Double -> io ()
        (/=) = modifyNum prim__assign_div
        export
        (%=) : HasIO io => Number -> Double -> io ()
        (%=) = modifyNum prim__assign_mod
        export
        (**=) : HasIO io => Number -> Double -> io ()
        (**=) = modifyNum prim__assign_pow


    public export
    data StringObject = ESStr AnyPtr String

    namespace StringObject
        %foreign bracketPropertyAccessor "" ""
        prim__get : AnyPtr -> String -> PrimIO String

        export
        get : HasIO io => StringObject -> io String
        get (ESStr parent name) = primIO $ prim__get parent name

        %foreign bracketPropertyAccessor ",x" "=x"
        prim__assign_set : AnyPtr -> String -> String -> PrimIO ()

        modifyStrObj : HasIO io 
                => (prim__assign_func : AnyPtr -> String -> String -> PrimIO ())
                -> StringObject 
                -> String 
                -> io ()
        modifyStrObj prim__assign_func (ESStr parent name) x = 
            primIO $ prim__assign_func parent name x

        infixl 8 ::=, ++=
        export
        (::=) : HasIO io => StringObject -> String -> io ()
        (::=) = modifyStrObj prim__assign_set

    -- for strings which can only be one of predefined values, defined through data on idris side
    public export
    data Data : (dat : Type) -> Type where
        ESDataStr : Show dat => StringObject -> Data dat
    
    namespace Data 
        -- TODO: Get
        
        infixl 8 ::=
        export
        (::=) : HasIO io => Data dat -> dat -> io ()
        (::=) (ESDataStr strobj) d = strobj ::= (show d)

    public export
    data Array : (content : Type) -> Type where
        ESArr : (parent : AnyPtr) -> (name : String) -> Array content

    namespace Array
        export
        get : HasIO io => Property.Array content -> io (List content)
        get (ESArr parent name) = do
            ptr <- primIO $ prim__access parent name
            let array = ref ptr 
            readToList array 
        
        %foreign bracketPropertyAccessor ",x" "=x"
        prim__assign_set : AnyPtr -> String -> ArrayPtr ty -> PrimIO ()

        infixl 8 ::=, ++=
        export
        (::=) : HasIO io => Property.Array content -> List content -> io ()
        (::=) (ESArr parent name) list =
            case !(newFromList list) of
                (MkESArray store) => primIO $ prim__assign_set parent name store


public export
interface ESEnum enumeratedType where
    store : enumeratedType -> PrimIO AnyPtr
    name : enumeratedType -> String

    enumerate : HasIO io => enumeratedType -> io Double
    enumerate e = primIO $ prim__get !(primIO $ store e) (name e)
    
    