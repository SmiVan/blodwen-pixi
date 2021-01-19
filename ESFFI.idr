module ESFFI

export
boolToInt : Bool -> Int
boolToInt True = 1
boolToInt False = 0


namespace Property
    bracketPropertyAccessor : (extraArgs: String) -> (suffix: String) -> String
    bracketPropertyAccessor extraArgs suffix = 
        "javascript:lambda: (p,n" ++ extraArgs ++ ") => p[n]" ++ suffix

    %foreign bracketPropertyAccessor "" ""
    export
    prim__access : AnyPtr -> String -> PrimIO AnyPtr

   
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

        infixl 8 ::=
        infixl 8 +=
        infixl 8 -=
        infixl 8 *=
        infixl 8 /=
        infixl 8 %=
        infixl 8 **=
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
    
    