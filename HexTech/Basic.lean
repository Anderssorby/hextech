import SDL
open SDL

namespace HexTech

structure Data where
  cont : Bool := true
  point : Point := {x := 0, y := 0}
  debugLog : Bool
  
def GameM := StateT Data IO
  deriving Monad, MonadState

instance : MonadLift IO GameM := ⟨StateT.lift⟩

open Event in
def handleEvent (event : Event) : GameM Unit := do
  let data ← get
  match event with
  | quitEvent => do
    if data.debugLog then println! "quit event"
    modifyGet λ data => ((), {data with cont := false : Data})
  | keyboardEvent ke => do
    if data.debugLog then println! "keyboard event {ke.timestamp}"
  | mouseMotionEvent mme => do
    if data.debugLog then println! "mouse motion event {mme.point}"
    modifyGet λ data => ((), {data with point := mme.point })
  | userEvent e => do
    if data.debugLog then println! "event {e.type}"
  | _ => do
    if data.debugLog then println! "event not catched"

open SDL.Event.Type in
def printCodes : IO Unit := do
  println! "Events:"
  println! "SDL_QUIT = {SDL_QUIT}"
  println! "SDL_MOUSEMOTION = {SDL_MOUSEMOTION}"
  println! "SDL_KEYDOWN = {SDL_KEYDOWN}"

partial def gameLoop (debugLog : Bool := false) : IO UInt32 := do
  let r ← SDL.init
  if r != 0 then
    IO.eprintln "Error in init"
  if debugLog then printCodes
  let window ← SDL.createWindow "Event" 800 500
  let renderer ← SDL.createRenderer window
  let bgrd ← SDL.loadImage "resources/jungle.png" >>= SDL.createTextureFromSurface renderer 
  let explosion ← SDL.loadImage "resources/rock.png" >>= SDL.createTextureFromSurface renderer
  let ast ← SDL.loadImage "resources/river.png" >>= SDL.createTextureFromSurface renderer
  let astR := { x := 0, y := 0, w := 95, h := 93 : Rect }
  renderer.setDrawColor Color.white
  let rec loop : GameM Unit := do
    let mut src := { x := 0, y := 0, w := 128, h := 128 }
    let dst := { x := 100, y := 100, w := 128, h := 128 }
    for i in [0:17] do
      Event.processEventQueue handleEvent 
      let data : Data ← get
      if !data.cont then break
      src := { src with x := (i * 128).toUInt32 }
      renderer.copy bgrd
      renderer.copy ast (some astR) (some <| astR.move data.point)
      renderer.copy explosion (some src) (some dst)
      renderer.present
      SDL.delay 100
    if (← get).cont then loop
  let (_, _d) ← loop.run { debugLog : Data }
  SDL.destroyWindow window
  SDL.quit
  return 0


