module PIXI
import ESFFI

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
    
    export
    rotation : Sprite -> Property.Number
    rotation (MkSprite c) = ESNum c "rotation"

    export
    x : Sprite -> Property.Number
    x (MkSprite c) = ESNum c "x"
    export
    y : Sprite -> Property.Number
    y (MkSprite c) = ESNum c "y"

    -- export
    -- width : Sprite -> Property.Number
    -- width (MkSprite s) = ESNum s "width"
    -- export
    -- height : Sprite -> Property.Number
    -- height (MkSprite s) = ESNum s "height"

export
data Container = MkContainer AnyPtr

namespace Container
    %foreign "browser:lambda: () => new PIXI.Container()"
    prim__newPixiContainer : PrimIO AnyPtr

    export
    new : HasIO io => io PIXI.Container
    new = map MkContainer $ primIO $ prim__newPixiContainer

    %foreign "browser:lambda: (container, sprite) => container.addChild(sprite)"
    prim__addChild : (container: AnyPtr) -> (sprite: AnyPtr) -> PrimIO ()

    export
    addChild : HasIO io => Container -> PIXI.Sprite -> io ()
    addChild (MkContainer container) (MkSprite sprite) =
        primIO $ prim__addChild container sprite

    export
    rotation : Container -> Property.Number
    rotation (MkContainer c) = ESNum c "rotation"

    export
    x : Container -> Property.Number
    x (MkContainer c) = ESNum c "x"
    export
    y : Container -> Property.Number
    y (MkContainer c) = ESNum c "y"

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
data Stage = MkStage AnyPtr

namespace Stage
    %foreign "browser:lambda: (stage, container) => stage.addChild(container)"
    prim__addChild : (stage: AnyPtr) -> (container: AnyPtr) -> PrimIO ()

    export
    addChild : HasIO io => Stage -> PIXI.Container -> io ()
    addChild (MkStage stage) (MkContainer container) =
        primIO $ prim__addChild stage container

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
    stage : HasIO io => (app : Application) -> io Stage
    stage (MkApplication anyptr) = map MkStage $ primIO $ prim__stage anyptr

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

