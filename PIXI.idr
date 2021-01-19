module PIXI
import ESFFI

data ObservablePoint = MkObsPoint AnyPtr

public export
interface DisplayObject a where
    (.internal) : a -> AnyPtr
    -- ++ Container
    -- Graphics
    -- ++ Sprite
    -- ++ Text
    -- BitmapText
    -- TilingSprite
    -- AnimatedSprite
    -- Mesh
    -- NineSlicePlane
    -- SimpleMesh
    -- SimplePlane
    -- SimpleRope

    -- (.pivot) : a -> ObservablePoint
    -- (.position) : a -> ObservablePoint
    -- (.scale) : a -> ObservablePoint
    -- (.skew) : a -> ObservablePoint
    
    (.x) : a -> Property.Number
    default_x : a -> Property.Number
    default_x obj = ESNum obj.internal "x"
    (.y) : a -> Property.Number
    default_y : a -> Property.Number
    default_y obj = ESNum obj.internal "y"
    (.rotation) : a -> Property.Number
    default_rotation : a -> Property.Number
    default_rotation obj = ESNum obj.internal "rotation"
    (.angle) : a -> Property.Number
    default_angle : a -> Property.Number
    default_angle obj  = ESNum obj.internal "angle" -- rotation in degrees

    -- WORKAROUND FOR https://github.com/idris-lang/Idris2/issues/954
    -- x = default_x
    -- y = default_y
    -- rotation = default_rotation
    -- angle = default_angle

export
data Texture = MkTexture AnyPtr

namespace Texture 
    %foreign "browser:lambda: (str) => PIXI.Texture.from(str)"
    prim__from : String -> PrimIO AnyPtr

    export
    from : HasIO io => String -> io PIXI.Texture
    from str = map MkTexture $ primIO $ prim__from str

export
data Sprite = MkSprite AnyPtr

export
DisplayObject Sprite where
    -- WORKAROUND FOR https://github.com/idris-lang/Idris2/issues/954
    x = default_x
    y = default_y
    rotation = default_rotation
    angle = default_angle
    internal (MkSprite i) = i

namespace Sprite
    %foreign "browser:lambda: (texture) => new PIXI.Sprite(texture)"
    prim__new : AnyPtr -> PrimIO AnyPtr

    export
    new : HasIO io => Texture -> io PIXI.Sprite
    new (MkTexture tex) = map MkSprite $ primIO $ prim__new tex

    %foreign "browser:lambda: (spr, pos) => spr.anchor.set(pos)" 
    prim__anchorset : AnyPtr -> Double -> PrimIO ()
    
    export
    anchorset : HasIO io => Sprite -> Double -> io ()
    anchorset (MkSprite spr) pos = primIO $ prim__anchorset spr pos

    -- %foreign "browser:lambda: (spr, blend) => spr.blendmode = PIXI.BLEND_MODES.ADD" 
    -- prim__blendmodeset : AnyPtr -> String -> PrimIO ()

    -- export
    -- blendmodeset : HasIO io => Sprite -> BlendMode -> io ()
    -- blendmodeset (MkSprite spr) blend = primIO $ prim__blendmodeset spr (show blend)
    

    %foreign "browser:lambda: x => x.blendMode = PIXI.BLEND_MODES.ADD"
    prim__blend_add : AnyPtr -> PrimIO ()

    export
    blendAdd : HasIO io => Sprite -> io ()
    blendAdd (MkSprite s) = primIO $ prim__blend_add s


export
data Text = MkText AnyPtr

export
DisplayObject Text where
    -- WORKAROUND FOR https://github.com/idris-lang/Idris2/issues/954
    x = default_x
    y = default_y
    rotation = default_rotation
    angle = default_angle
    internal (MkText i) = i
    
namespace Text
    
    %foreign "browser:lambda: (str) => new PIXI.Text(str)"
    prim__new : String -> PrimIO anyPtr

    export
    new : HasIO io => String -> io PIXI.Text
    new str = map MkText $ primIO $ prim__new str


export
data Container = MkContainer AnyPtr

export
DisplayObject Container where
    -- WORKAROUND FOR https://github.com/idris-lang/Idris2/issues/954
    x = default_x
    y = default_y
    rotation = default_rotation
    angle = default_angle
    internal (MkContainer i) = i

namespace Container
    %foreign "browser:lambda: () => new PIXI.Container()"
    prim__newPixiContainer : PrimIO AnyPtr

    export
    new : HasIO io => io PIXI.Container
    new = map MkContainer $ primIO $ prim__newPixiContainer

    %foreign "browser:lambda: (container, displayObject) => container.addChild(displayObject)"
    prim__addChild : (container: AnyPtr) -> (displayObject: AnyPtr) -> PrimIO ()

    export
    addChild : HasIO io => DisplayObject displayObject 
        => Container -> displayObject -> io ()
    addChild (MkContainer container) displayObject =
        primIO $ prim__addChild container displayObject.internal

    export
    data Pivot = MkPivot AnyPtr

    namespace Pivot
        export
        x : Pivot -> Property.Number
        x (MkPivot p) = ESNum p "x"
        export
        y : Pivot -> Property.Number
        y (MkPivot p) = ESNum p "y" 
    
    %foreign "browser:lambda: (con) => con.pivot"
    prim__pivot : AnyPtr -> PrimIO AnyPtr

    export
    pivot : HasIO io => Container -> io Pivot
    pivot (MkContainer c) = map MkPivot $ primIO $ prim__pivot c

export
data Application = MkApplication AnyPtr
    
namespace Application     
    %foreign "browser:lambda: 
        (backgroundColor, transparent) => 
        new PIXI.Application({
            backgroundColor: Number(backgroundColor), 
            transparent: transparent,
            resizeTo: window
        })"
    prim__newPixiApplication : (backgroundColor: Int) 
                            -> (transparent : Int)
                            -> PrimIO AnyPtr

    export
    new : HasIO io 
        => {default 0 color : Int} 
        -> {default False transparent : Bool} 
        -> {default False transparent : Bool} 
        -> io PIXI.Application
    new {color} {transparent} = 
        map MkApplication $ primIO $ prim__newPixiApplication color (boolToInt transparent)

    %foreign "browser:lambda: (app) => app.view"
    prim__view : AnyPtr -> PrimIO AnyPtr

    export
    view : HasIO io => (app : Application) -> io AnyPtr
    view (MkApplication anyptr) = primIO $ prim__view anyptr

    %foreign "browser:lambda: (app) => app.stage"
    prim__stage : AnyPtr -> PrimIO AnyPtr

    export 
    stage : HasIO io => (app : Application) -> io Container
    stage (MkApplication anyptr) = map MkContainer $ primIO $ prim__stage anyptr

    %foreign "browser:lambda: (app, callback) => app.ticker.add((delta) => callback(delta)())"
    prim__tickeradd : AnyPtr -> (Double -> PrimIO ()) -> PrimIO () 

    export
    tickerAdd : HasIO io => Application -> (Double -> IO ()) -> io ()
    tickerAdd (MkApplication app) callback = do
        primIO $ prim__tickeradd app (\d => toPrim $ callback d)

    %foreign "browser:lambda: (app) => app.ticker.lastTime"
    prim__tickerlasttime : AnyPtr -> PrimIO Double 

    export
    tickerLastTime : HasIO io => Application -> io Double
    tickerLastTime (MkApplication app) = primIO $ prim__tickerlasttime app

    %foreign "browser:lambda: (app) => app.resize()"
    prim__resize : AnyPtr -> PrimIO ()

    export
    resize : HasIO io => Application -> io ()
    resize (MkApplication app) = primIO $ prim__resize app

    export
    data Screen = MkScreen AnyPtr

    namespace Screen
        export
        width : Screen -> Property.Number
        width (MkScreen s) = ESNum s "width"
        export
        height : Screen -> Property.Number
        height (MkScreen s) = ESNum s "height"

    %foreign "browser:lambda: (app) => app.screen"
    prim__screen : AnyPtr -> PrimIO AnyPtr

    export
    screen : HasIO io => (app : Application) -> io Screen
    screen (MkApplication anyptr) = map MkScreen $ primIO $ prim__screen anyptr

