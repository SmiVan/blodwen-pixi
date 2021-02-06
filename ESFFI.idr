module ESFFI

export
Cast Bool Double where
    cast True = 1.0
    cast False = 0.0

export
Cast Double Bool where
    cast 1.0 = True
    cast _ = False

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
interface ESEnum enumeratedType where
    store : enumeratedType -> PrimIO AnyPtr
    name : enumeratedType -> String

    enumerate : HasIO io => enumeratedType -> io Double
    enumerate e = primIO $ prim__get !(primIO $ store e) (name e)
    
    