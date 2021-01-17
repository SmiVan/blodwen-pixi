module ESFFI

export
boolToInt : Bool -> Int
boolToInt True = 1
boolToInt False = 0

namespace Property
    public export
    data Number = ESNum AnyPtr String

    namespace Number

        %foreign "javascript:lambda: (p,n) => p[n]"
        prim__get : AnyPtr -> String -> PrimIO Double

        %foreign "javascript:lambda: (p,n,x) => p[n]=x"
        prim__assign_set : AnyPtr -> String -> Double -> PrimIO ()
        %foreign "javascript:lambda: (p,n,x) => p[n]+=x"
        prim__assign_add : AnyPtr -> String -> Double -> PrimIO ()
        %foreign "javascript:lambda: (p,n,x) => p[n]-=x"
        prim__assign_sub : AnyPtr -> String -> Double -> PrimIO ()
        %foreign "javascript:lambda: (p,n,x) => p[n]*=x"
        prim__assign_mul : AnyPtr -> String -> Double -> PrimIO ()
        %foreign "javascript:lambda: (p,n,x) => p[n]/=x"
        prim__assign_div : AnyPtr -> String -> Double -> PrimIO ()
        %foreign "javascript:lambda: (p,n,x) => p[n]%=x"
        prim__assign_mod : AnyPtr -> String -> Double -> PrimIO ()
        %foreign "javascript:lambda: (p,n,x) => p[n]**=x"
        prim__assign_pow : AnyPtr -> String -> Double -> PrimIO ()

        
        export
        get : HasIO io => Number -> io Double
        get (ESNum parent name) = primIO $ prim__get parent name

        infixl 8 ::=
        infixl 8 +=
        infixl 8 -=
        infixl 8 *=
        infixl 8 /=
        infixl 8 %=
        infixl 8 **=
        export
        (::=) : HasIO io => Number -> Double -> io ()
        (::=) (ESNum parent name) x = primIO $ prim__assign_set parent name x
        export
        (+=) : HasIO io => Number -> Double -> io ()
        (+=) (ESNum parent name) x = primIO $ prim__assign_add parent name x
        export
        (-=) : HasIO io => Number -> Double -> io ()
        (-=) (ESNum parent name) x = primIO $ prim__assign_sub parent name x
        export
        (*=) : HasIO io => Number -> Double -> io ()
        (*=) (ESNum parent name) x = primIO $ prim__assign_mul parent name x
        export
        (/=) : HasIO io => Number -> Double -> io ()
        (/=) (ESNum parent name) x = primIO $ prim__assign_div parent name x
        export
        (%=) : HasIO io => Number -> Double -> io ()
        (%=) (ESNum parent name) x = primIO $ prim__assign_mod parent name x
        export
        (**=) : HasIO io => Number -> Double -> io ()
        (**=) (ESNum parent name) x = primIO $ prim__assign_mul parent name x
