prelude = require 'prelude-ls'

# Helpers

V3    = (x,y,z) -> new THREE.Vector3(x,y,z)
mod   = (a,b)   -> ((a%b) + b) % b
clamp = (a,b,x) -->
  | x < a => a
  | x > b => b
  | _     => x 


# Basics

Vis = new ->
  @newTriGeom = (v1,v2,v3) ->
    # throw new Error "Expecting three vertices" unless vs.length == 3

    geom = new THREE.Geometry()

    geom.vertices.push v1
    geom.vertices.push v2
    geom.vertices.push v3

    geom.faces.push new THREE.Face3( 0, 1, 2 )
    geom.computeFaceNormals!
    geom.computeBoundingSphere!

    geom

  @newTriMesh = (geom, color=0xffffff) ->
    # mesh = new THREE.Mesh geom, new THREE.MeshNormalMaterial()
    mesh = new THREE.Mesh geom, new THREE.MeshBasicMaterial(color: color)

  @triBaseLength   = 1.0
  @triRatio        = 1 / 1.61
  @triBaseHeight   = @triBaseLength * @triRatio

  @triGeom         = let l = @triBaseLength, h = @triBaseHeight
    @newTriGeom (V3 -l/2,0,0), (V3 l/2,0,0), (V3 0,h,0)
  # @triGeomFlipped  = let l = @triBaseLength, h = @triBaseHeight
  #   @newTriGeom (V3 0,0,0), (V3 l/2,h,0), (V3 -l/2,h,0)

  @translateGeom   = (geom, vAdd) ->
    geom.vertices.forEach(-> it.add(vAdd))
    geom
  @computeFlipped  = (other) ~>
    vMul = V3 1, -1, 1
    vAdd = V3 0, other.vertices[2].y, 0 # 0, @triBaseHeight, 0
    [v1,v2,v3] = other.vertices.map(~> it.clone!.multiply(vMul).add(vAdd))
    @newTriGeom v3, v2, v1
  @triGeomFlipped  = @computeFlipped @triGeom

  @computeScaled   = (n) ~>
    fact  = 1 / Math.exp(2,n)
    vX    = V3 (@triBaseLength * fact * 0.5), 0, 0
    vY    = V3 0, (@triBaseHeight * fact), 0
    [v1,v2,v3] = @triGeom.vertices.map(-> it.clone!.multiplyScalar(fact))
    @newTriGeom (v1.sub vX), (v2.add vX), (v3.add vY)
  @triGeomSmall    = @computeScaled 1
  @triGeomSmallFlipped = @computeFlipped @triGeomSmall

  @triBase = (cx,cy) ->
    geom      = | cx % 2 is 0 => @triGeom
                | _           => @triGeomFlipped
    with (@newTriMesh geom)
      # ..position.set mx, my, 0
      # ..position.set 0, 0, 0
      ..renderDepth = (cx % 2)
      # ..renderDepth = if cx == 0 then 0 else 10 #Math.random! * 1000.0
      # ..position.setZ (Math.random! / 1.0)
      ..material.side = THREE.DoubleSide # Performance hit?
    # @newTriMesh geom

  @triMeshes = (callback) !->
    # ts = []
    # for x from -25 to 24
    #     for y from -10 to 10
    for x from -26 to 25
        for y from -13 to 12
    # for x from -35 to 34
    #     for y from -16 to 16
          callback (@triBase x,y), x, y
    # ts

  @triBaseDefaultParams =
    oddExtra  : 1.0
    staggerAB : 1.0
    staggerA  : 1.0
    staggerB  : 1.0
  @updateTriMeshes = (visState) !->
    params    = visState.params
    stagger   = @triBaseLength / 2 * params.staggerAB
    oddExtra  = params.oddExtra #1.0

    updateMesh = (mesh, cx,cy) !~>
      # stagger2 = stagger * (1 + (cx<0 ? -cx : cx) * 0.1)
      cxp       = cx
      # cxp       = Math.pow cx, 1.1
      mx        = | cy % 2 is 0 => cxp * stagger * params.staggerA
                  | _           => (cxp + oddExtra) * stagger * params.staggerB
      my        = cy * @triBaseHeight
      color     = 0xffffff
      visible   = true

      # visible = cx % 2 == 0
      # visible =
      #   | cy % 2 == 0 => cx % 8 != 3
      #   | _           => cx % 8 == 1
      color = 0x00ffff if cx % 2 == 0

      mesh
        ..position.set mx, my, 0        if mesh.position.x != mx || mesh.position.y != my
        ..visible = visible             if mesh.visible != visible
        ..material.color.setHex(color)  if mesh.material.color.getHex! != color
        # ..rotation.z = mesh.rotation.z + 0.01
        # ..renderDepth = 10 + (cx % 2)

    visState.trisByCoord updateMesh

  @miscDefaultParams =
    effects:
      bloom:      false
      dotScreen:  false
      glitch:     false

# test : (Int,Int) -> VisState -> Element
# test (w,h) {time, tpb, phase} = 
#   --[triBase (Coord 0 0), triBase (Coord 1 0), triBase (Coord 0 1)]
#   let
#     --(w,h) = (640,360)
#     beatsPerPeriod = 4.0
#     --tpb!  = case tpb of
#     --  Just x  -> x
#     --  Nothing -> 1
#     p     = periodFrac (tpb * beatsPerPeriod) (time - phase)
#     ts    = tris blue (22*2) 20
#     --ts    = tris blue (22*2*2) (20*2)
#     --color = hsl 0 255 (sin (2*pi*p) * 0.25 + 0.75) 
#     color = hsl p 1.0 0.75 
#     --color = greyscale (sin (2*pi*p) * 0.5 + 0.5)
#     --ets   = evenTris ts |> colorTris color
#     --q     = (toFloat (ceiling (4*p))) / 4.0
#     q     = p
#     ets   = evenTris ts 
#       |> colorTris yellow
#       --|> filter (\(Tri (Coord x y) _) -> x == y || (x+1) == y)  
#       |> filter (\(Tri (Coord x y) _) -> x == y)  
#       |> displaceTris (q*triBaseLength * beatsPerPeriod, 0)
#     ots   = oddTris ts
#   in
#     collage (w-200) h <| triForms <| ets ++ ots


BEATS_PER_BAR = 4

class VisState
  (@time=0.0, @dt=0, @running=true, @perspCam=false, @bpm=128.0, @phase=0.0, params) ->
    @clearTris!

    defaultParams = ({} <<< Vis.triBaseDefaultParams) <<< Vis.miscDefaultParams
    @params = defaultParams <<< (@params || {})

    @seqs = [new MeteorSequence(), new WaveSequence(), new CircleSequence(), 
      new ShearSequence(), new TranslateSequence(), new SidestepSequence()]
    @activeSeqIndex = void
    # @activeSeqSetup = false
    @nextSeqIndex   = 0
    @nextSeqStart   = 0


  step: (dt) ->
    return this unless @running
    @time += dt
    @dt   = dt
    
    # Vis.updateTriMeshes this
    
    # Callbacks for the active sequence
    if @nextSeqIndex isnt void && @time >= @nextSeqStart
      @seqs[@activeSeqIndex].teardown this if @activeSeqIndex isnt void
      @activeSeqIndex = @nextSeqIndex
      @seqs[@activeSeqIndex].setup this
      @nextSeqIndex = void
      @nextSeqStart = void
      @phase        = @phase % (BEATS_PER_BAR * @tpb)

    @seqs[@activeSeqIndex].step this

    this

  scheduleSequence: (seqIndex) ->
    @nextSeqIndex = seqIndex
    @nextSeqStart = @time + @tpb * BEATS_PER_BAR * (1.0 - @pBar)

  clearTris: !->
    # @tris = new Hashtable()
    @tris = []

  coord2key: (cx,cy)  -> ((cx+500)*1000 + (cy+500))
  key2coord: (key)    -> [Math.floor(key/1000)-500, (key%1000)-500]

  addTri: (mesh,cx,cy) !->
    # # @tris[@coord2key(cx,cy)] = mesh
    # @tris.put @coord2key(cx,cy), mesh
    @tris.push [cx,cy,mesh]

  trisByCoord: (callback) !->
    # # for key, mesh of @tris
    # #   [cx, cy] = @key2coord(key)
    # #   callback mesh, cx, cy
    # #   # if cx + cy is NaN
    # #   #   console.log key, [cx,cy]
    # #   #   return
    # #   console.log key, [cx,cy]
    # @tris.each (key, mesh) ~>
    #   [cx, cy] = @key2coord(key)
    #   callback mesh, cx, cy
    for [cx,cy,mesh] in @tris
      callback mesh, cx, cy

  bpm:~ # beats per minute (translated from time per beat)
    -> 60.0 * 1000.0 / @tpb
    (bpm) !-> @tpb = 60.0 * 1000.0 / bpm

  vTime:~ # virtual time (corrected by phase)
    -> @time - @phase

  pBeat:~ # percent of current beat
    -> (@vTime % @tpb) / @tpb

  vBeat:~ # pBeat, not limited to 100%
    -> @vTime / @tpb

  vBarBeat:~ # pBeat, limited to 400% (i.e., according to bar)
    -> (@vTime % (BEATS_PER_BAR * @tpb)) / @tpb

  pBar:~ # percent of current bar
    -> let f = BEATS_PER_BAR * @tpb
      (@vTime % f) / f

  vBar:~ # pBar, not limited to 100%
    -> let f = BEATS_PER_BAR * @tpb
      @vTime / f

  vBarBeatToDelta: (vBB) -> (vBB * @tpb)

  vBarBeatToTime: (vBB) -> let barLen = (BEATS_PER_BAR * @tpb)
    @vTime - (@vTime % barLen) + barLen + (vBB * @tpb)


class Tri
  ->
    # Large (normal)
    geom      = | cx % 2 is 0 => @triGeom
                | _           => @triGeomFlipped
    @mLarge = Vis.newTriMesh geom

    # Small
    vBase     = V3 @triBaseLength, @triBaseHeight, 0
    @msSmall  = [(V3 -1,0,0), (V3 1,0,0), (V3 0,1,0)].map ->
      it.multiply(vBase) ...


class Sequence
  ->
    @params = @defaultParams!

  setup: (visState) ->
    # virtual
  step: (visState) ->
    # virtual
  teardown: (visState) ->
    # virtual

  basicGridDefaultParams: ->
    oddExtra  : 1.0
    staggerAB : 1.0
    staggerA  : 1.0
    staggerB  : 1.0
    # ySpreadAB : 0.0
    xAddFunc  : void
    yAddFunc  : void
    cxFunc    : void
    cyFunc    : void
  buildBasicGridUpdater: (params={}) ->
    params    = ({} <<< @basicGridDefaultParams!) <<< params
    stagger   = Vis.triBaseLength / 2 * params.staggerAB
    oddExtra  = params.oddExtra

    updateMesh = (mesh, cx,cy) !~>
      cxt      = if params.cxFunc then params.cxFunc cx, cy else cx
      cyt      = if params.cyFunc then params.cyFunc cx, cy else cy
      
      mx        = | cy % 2 is 0 => cxt * stagger * params.staggerA
                  | _           => (cxt + oddExtra) * stagger * params.staggerB
      mx        += if params.xAddFunc then params.xAddFunc cx, cy else 0

      my        = cyt * Vis.triBaseHeight #cx * params.ySpreadAB
      my        += if params.yAddFunc then params.yAddFunc cx, cy else 0
      
      visible   = true

      color     = 0xffffff
      color = 0x00ffff if cx % 2 == 0
      # color = 0x000000 if cx != 0

      mesh
        ..position.set mx, my, 0        if mesh.position.x != mx || mesh.position.y != my
        ..visible = visible             if mesh.visible != visible
        ..material.color.setHex(color)  if mesh.material.color.getHex! != color

  beatRangeProgress: (vBeat,a,b) ->
    return void unless vBeat >= a && vBeat <= b
    return (vBeat - a) / (b - a)



# class ManualSequence extends Sequence
#   # ->
#   #   super @defaultParams!

#   defaultParams: ->
#     oddExtra  : 1.0
#     staggerAB : 1.0
#     staggerA  : 1.0
#     staggerB  : 1.0

#   step: (visState) ->
#     params    = visState.params
#     stagger   = @triBaseLength / 2 * params.staggerAB
#     oddExtra  = params.oddExtra #1.0

#     updateMesh = (mesh, cx,cy) !~>
#       # stagger2 = stagger * (1 + (cx<0 ? -cx : cx) * 0.1)
#       cxp       = cx
#       # cxp       = Math.pow cx, 1.1
#       mx        = | cy % 2 is 0 => cxp * stagger * params.staggerA
#                   | _           => (cxp + oddExtra) * stagger * params.staggerB
#       my        = cy * @triBaseHeight
#       color     = 0xffffff
#       visible   = true

#       # visible = cx % 2 == 0
#       # visible =
#       #   | cy % 2 == 0 => cx % 8 != 3
#       #   | _           => cx % 8 == 1
#       color = 0x00ffff if cx % 2 == 0
#       color = 0x000000 if cx != 0

#       mesh
#         ..position.set mx, my, 0        if mesh.position.x != mx || mesh.position.y != my
#         ..visible = visible             if mesh.visible != visible
#         ..material.color.setHex(color)  if mesh.material.color.getHex! != color
#         # ..rotation.z = mesh.rotation.z + 0.01
#         # ..renderDepth = 10 + (cx % 2)

#     visState.trisByCoord updateMesh

class SidestepSequence extends Sequence
  defaultParams: ->
    rpBar: 0.5

  setup: (visState) ->
    visState.trisByCoord @buildBasicGridUpdater!

  step: (visState) ->
    angle = visState.vBar * @params.rpBar * -2 * Math.PI
    # beat = Math.floor(visState.vBeat)
    beat = visState.vBeat % 4
    
    updateMesh = (mesh, cx,cy) !~>
      # mesh.rotation.z = angle
      # return unless cy % 2 == 0
      
      # (mod cx, 4) == beat
      defaultColor = new THREE.Color 0xffffff #(if cx % 2 == 0 then 0x00ffff else 0xffffff)
      activeColor  = new THREE.Color 0x00ffff #0x000000
      
      # dist  = mod (cx - beat), 4 |> Math.abs |> clamp 0, 1
      dist0 = (mod cx, 4) - beat
      dist  = Math.min(dist0 |> Math.abs, dist0 + 4 |> Math.abs) |> clamp 0, 1
      
      color = defaultColor.lerp activeColor, 1.0 - (dist/1.0)
      mesh.material.color.set(color)

    visState.trisByCoord updateMesh

class WaveSequence extends Sequence
  defaultParams: ->
    rpBar: 0.5

  setup: (visState) ->
    visState.trisByCoord @buildBasicGridUpdater!

  teardown: (visState) ->
    console.log \TEARDOWN
    visState.trisByCoord (mesh, cx,cy) ->
      mesh.rotation.y = 0

  step: (visState) ->
    # angle = visState.vBar * @params.rpBar * -2 * Math.PI
    # beat = Math.floor(visState.vBeat)
    beat4 = visState.vBeat % 4
    
    updateMesh = (mesh, cx,cy) !~>
      return unless cx % 2 == 0

      # p = (mod ((cx + 20) / 40), 1.0)
      # p = (mod ((cx + 30) / 60), 1.0)
      p = ((cx + 30) / 60)
      # p = ((cx + 28) / 56)
      return unless p > 0.0 && p < 1.0
      # q = @beatRangeProgress beat4, p
      q = (beat4 / 4.0 - p)
      # q = 1.0 - (q * q)
      q = 1.0 - q
      # console.log beat4, q if cx == 0 and cy == 0
      angle = -2 * Math.PI * q

      mesh.rotation.y = angle

    visState.trisByCoord updateMesh

class ShearSequence extends Sequence
  defaultParams: -> {}

  # setup: (visState) ->
  #   visState.trisByCoord @buildBasicGridUpdater!

  step: (visState) ->
    beat    = visState.vBeat
    
    barNum  = Math.floor(visState.vBar)
    # sign    = if barNum % 2 == 0 then 1 else -1
    onBar   = barNum % 2 == 1

    BEAT_PERIOD = 8
    # expo    = (beat % 8) / 16.0
    expo    = Math.cos(((beat + 3.0) % BEAT_PERIOD) / BEAT_PERIOD * 2 * Math.PI) / 8.0 + 0.125 # + 0.25
    # expo    = Math.pow( Math.cos((beat % BEAT_PERIOD) / BEAT_PERIOD * 2 * Math.PI), 2) / 4.0 + 0.0
    # b       = (beat % BEAT_PERIOD) / BEAT_PERIOD
    # expo    = 
    #   | b < 0.5   => TWEEN.Easing.Quadratic.In(b*2)
    #   | b >= 0.5  => 1.0 - TWEEN.Easing.Quadratic.Out((b-0.5)*2)
    # expo    = expo / 4.0 #+ 0.25
    expo1   = 1.0 + expo


    p = @beatRangeProgress(beat%4, 0.0, 1.0) # 3.0, 4.0
    p0 = TWEEN.Easing.Quadratic.InOut(p || 0)

    # updater = @buildBasicGridUpdater!
    basicUpdater = @buildBasicGridUpdater {
      cxFunc: (cx,cy) ->
        | cx < 0  => -(Math.pow -cx, expo1) + cx * expo * 2.0
        | _       => (Math.pow cx, expo1) + cx * expo * 2.0
      cyFunc: (cx,cy) -> 
        | cx >= -1 && cx <= 1                 => cy
        | barNum % 4 == 1 && (mod cx, 2) == 0 => cy + 2*p0
        | barNum % 4 == 3 && (mod cx, 2) == 1 => cy - 2*p0
        | _                                   => cy
    }

    # beat4 = beat % 4
    # beatY = ((beat % 4) - 3.0) * 10 |> Math.floor

    visState.trisByCoord (mesh, cx,cy) ->
      basicUpdater mesh, cx,cy
      color = if cx % 2 == 0 then 0x00ffff else 0xffffff

      # return unless barNum % 4 == 3
      # return unless onBar

      # sign  = if cx % 2 == 0 then 1 else -1
      # if beatY >= -10 && cy == beatY * sign
      #   color = if cx % 2 == 0 then 0xff00ff else 0xffff00
      if p isnt void
        color = 0xff00ff if barNum % 4 == 1 && (mod cx, 2) == 0
        color = 0xffff00 if barNum % 4 == 3 && (mod cx, 2) == 1
      # color = if cx % 2 == 0 then 0xff00ff else 0xffff00
      
      mesh.material.color.setHex(color)

class TranslateSequence extends Sequence
  defaultParams: -> {}

  # setup: (visState) ->
  #   visState.trisByCoord @buildBasicGridUpdater!

  step: (visState) ->
    beat    = visState.vBeat
    beat4   = beat % 4
    barNum  = Math.floor(visState.vBar)
    sign    = if barNum % 2 == 0 then 1 else -1

    updater = @buildBasicGridUpdater {
      staggerAB: beat4 / 4.0 + 1.0,
      # xAddFunc: (cx,cy) -> beat4*4 * sign * Vis.triBaseLength
      yAddFunc: (cx,cy) -> cx * sign * beat4 / 8.0
      # yAddFunc: (cx,cy) -> Math.sin(beat / 8.0 * Math.PI) * cx #* sign
    }

    visState.trisByCoord updater

class CircleSequence extends Sequence
  defaultParams: -> {
    minHWidth: 0.5,
    maxHWidth: 3.0,
    pulse: false
  }

  setup: (visState) ->
    visState.trisByCoord @buildBasicGridUpdater!

  step: (visState) ->
    beat    = visState.vBeat
    barNum  = Math.floor(visState.vBar)
    beat8norm = (beat % 8) / 8.0
    beat8normPingPong = (if barNum % 2 == 0 then beat8norm else 1.0 - beat8norm)
    R         = 30.0

    # Radii and Colors
    # rcs     = [ [beat8norm * R, 0xff0000] ]
    # rcs     = [ [(beat8norm + 0.25 * i) * R, 0x00ffff] for i from 0 til 3 ]
    # rcs     = [ [(beat8norm + 0.25 * (Math.exp(2,i) - 1)) * R, 0x00ffff] for i from 0 til 3 ]
    cs      = [ 0x007777, 0x009999, 0x00bbbb, 0x00ffff, 0x00bbbb, 0x009999, 0x007777 ]
    rcs     = [ [(beat8norm + 0.25 * i) * R, cs[i+3]] for i from -3 to 3 ]
    rcs.length -= 3 if visState.vBar < 2.0 # Hide the 'negative' rings in the first iteration
    
    # Half width of the rings
    # hWidth  = beat8normPingPong * 2.5 + 0.5
    hWidth  = beat8normPingPong * (@params.maxHWidth - @params.minHWidth) + @params.minHWidth

    basicUpdater = @buildBasicGridUpdater {
      # staggerAB: (beat % 8) / 4.0 + 1.0,
      # staggerAB: (beat % 8) / 8.0 + 1.0,
      staggerAB: if @params.pulse then beat8normPingPong * 0.5 + 1.1 else 1.2,
      # xAddFunc: (cx,cy) -> ...
    }

    visState.trisByCoord (mesh, cx,cy) ->
      basicUpdater mesh, cx,cy

      # return unless cx % 2 == 0
      # color = if barNum % 2 == 0 then 0xff0000 else 0x00ff00

      d = Math.sqrt(cx*cx + cy*cy)
      f = ([r,c]) -> (r >= 0) && (d > r - hWidth && d < r + hWidth)

      foundRc = prelude.find f, rcs
      color   = if foundRc then foundRc[1] else 0x000000

      mesh.material.color.setHex color if mesh.material.color.getHex! isnt color

class MeteorSequence extends Sequence
  defaultParams: -> {
    spawnWait: 1.0,
    v:        64, # per Bar
    pSign1:   0.95,
    pChange:  0.001
  }

  setup: (visState) ->
    visState.trisByCoord @buildBasicGridUpdater!
    
    @nextShot = void
    @ms       = []
    # @d        = 0

  step: (visState) ->
    beatNum = Math.floor(visState.vBeat % 4)
    
    if @nextShot is void || visState.vTime >= @nextShot
      mx = Math.floor(Math.random! * 40 - 20.0)
      my = Math.floor(Math.random! * 20 - 30.0)
      s  = if Math.random! < @params.pSign1 then 1 else -1
      @ms.push [mx,my,0,s]
      # @nextShot = visState.vBarBeatToTime 0.0
      @nextShot = visState.vTime + visState.vBarBeatToDelta @params.spawnWait
      @nextStep = visState.vTime + visState.vBarBeatToDelta (4.0 / @params.v) 
    
    if visState.vTime >= @nextStep
      for m, i in @ms
        [mx,my,d,s] = m
        mx += (d % 2) * s
        d  = (d+1) % 2
        my += d % 2
        s *= -1 if Math.random! < @params.pChange
        @ms[i] = [mx,my,d,s]
      @ms = @ms.filter ([x,y,_,_]) -> Math.abs(x) < 30 && Math.abs(y) < 30
      @nextStep = visState.vTime + visState.vBarBeatToDelta (4.0 / @params.v) 

    basicUpdater = @buildBasicGridUpdater {
      # staggerAB: (beat % 8) / 4.0 + 1.0,
      # staggerAB: (beat % 8) / 8.0 + 1.0,
      # staggerAB: if @params.pulse then beat8normPingPong * 0.5 + 1.1 else 1.2,
      # xAddFunc: (cx,cy) -> ...
    }

    visState.trisByCoord (mesh, cx,cy) ~>
      # basicUpdater mesh, cx,cy

      color = mesh.material.color.multiplyScalar(0.99).getHex!
      color = 0x00ffff if prelude.find ((m) -> m[0] == cx && m[1] == cy), @ms
      
      mesh.material.color.setHex color #if mesh.material.color.getHex! isnt color




# Main

main = !->

  var stats
  var camera, scene, renderer, composer
  var object, light
  var renderPass, effectCopy, passes, passesOrder
  var visState, visStateGui


  # Canvas dimensions and camera parameters

  var canvasWidth, canvasHeight, canvasRatio, cameraParams, perspCamActive
  fetchCanvasSize = !->
    # width         = 24 #36
    width         = 48
    canvasWidth   := document.getElementById(\renderArea).clientWidth #window.innerWidth
    canvasHeight  := document.getElementById(\renderArea).clientHeight #window.innerHeight
    canvasRatio   := canvasWidth / canvasHeight
    cameraParams  :=
      let height = width / canvasRatio
        left: width / - 2, right: width / 2, 
        top: height / 2, bottom: height / - 2,
        near: -10, far: 10

  dpr = if window.devicePixelRatio isnt void then window.devicePixelRatio else 1
  sampleRatio = 2 # for super sampling


  # Engine stuff

  init = !->
    fetchCanvasSize!

    renderer := new THREE.WebGLRenderer()
    renderer.setSize canvasWidth, canvasHeight
    renderer.autoClear = false #???
    # document.body.appendChild renderer.domElement
    document.getElementById(\renderArea).appendChild renderer.domElement

    # Stats

    stats := new Stats()
    stats.domElement.style.position = 'absolute'
    stats.domElement.style.bottom = '0px'
    stats.domElement.style.top = '0px'
    stats.domElement.style.zIndex = 100
    document.body.appendChild stats.domElement

    # State

    visState := new VisState()

    # Camera and Scene

    # camera := new THREE.PerspectiveCamera( 24, canvasRatio, 10, 50 );
    # # camera.position.z = 36
    # camera.position.x = 17
    # camera.position.z = 15.0
    # camera.rotation.y = 0.9
    # camera := let p = cameraParams
    #   new THREE.OrthographicCamera p.left, p.right, p.top, p.bottom, p.near, p.far

    scene := new THREE.Scene()
    # scene.fog = new THREE.Fog( 0x000000, 1, 1000 )


    # geometry = new THREE.SphereGeometry( 1, 4, 4 )
    # # material = new THREE.MeshPhongMaterial( color: 0xffffff, shading: THREE.FlatShading )

    # for i from 0 til 100
    #   # material = new THREE.MeshPhongMaterial( { color: 0xffffff * Math.random(), shading: THREE.FlatShading } )
    #   material = new THREE.MeshBasicMaterial(color: 0x00ff00)

    #   mesh = new THREE.Mesh( geometry, material )
    #   mesh.position.set( Math.random() - 0.5, Math.random() - 0.5, Math.random() - 0.5 ).normalize()
    #   mesh.position.multiplyScalar( Math.random() * 400 )
    #   mesh.rotation.set( Math.random() * 2, Math.random() * 2, Math.random() * 2 )
    #   mesh.scale.x = mesh.scale.y = mesh.scale.z = Math.random() * 50
    #   object.add( mesh )

    rebuildTris!


    # scene.add( new THREE.AmbientLight( 0x222222 ) )
    
    # light := new THREE.DirectionalLight( 0xffffff )
    # light.position.set( 1, 1, 1 )
    # scene.add( light )


    # PostProcessing

    composer := new THREE.EffectComposer( renderer )
    composer.setSize canvasWidth * dpr * sampleRatio, canvasHeight * dpr * sampleRatio
    
    renderPass := new THREE.RenderPass( scene, null )
    composer.addPass( renderPass )

    # fxaaPass := new THREE.ShaderPass(THREE.FXAAShader)
    # fxaaPass.uniforms['resolution'].value.set(1 / (window.innerWidth * dpr), 1 / (window.innerHeight * dpr))
    # fxaaPass.renderToScreen = true
    # composer.addPass( fxaaPass )

    bloomPass = new THREE.BloomPass() #(2.0, 8, 2.0)
    bloomPass.renderToScreen = false
    bloomPass.enabled = false
    composer.addPass( bloomPass )

    effectCopy := new THREE.ShaderPass(THREE.CopyShader)
    effectCopy.renderToScreen = true
    composer.addPass( effectCopy )

    dotScreenPass = new THREE.DotScreenPass()
    dotScreenPass.renderToScreen = false
    dotScreenPass.enabled = false
    composer.addPass( dotScreenPass )

    glitchPass = new THREE.GlitchPass(32)
    glitchPass.goWild = true
    glitchPass.renderToScreen = false
    glitchPass.enabled = false
    composer.addPass( glitchPass )

    passes := 
      bloom:      bloomPass
      dotScreen:  dotScreenPass
      glitch:     glitchPass
    passesOrder := [\bloom, \dotScreen, \glitch]

    # refreshEffectPasses!

    onWindowResize!


    # Params GUI

    visStateGui := with (new dat.GUI())
      ..add(visState, 'time').listen!
      ..add(visState, 'running').listen!
      ..add(visState, 'perspCam').onFinishChange onWindowResize
      # ..add(visState, 'activeSeqIndex').listen!
      ..add(visState, 'vBarBeat').listen!
      ..add(visState, 'bpm')
      ..addFolder('triBase')
        ..add(visState.params, 'oddExtra', -5, 5).onFinishChange rebuildTris
        ..add(visState.params, 'staggerAB', -2, 2).onFinishChange rebuildTris
        ..add(visState.params, 'staggerA', -2, 2).onFinishChange rebuildTris
        ..add(visState.params, 'staggerB', -2, 2).onFinishChange rebuildTris
      ..addFolder('effects')
        ..add(visState.params.effects, 'bloom').onFinishChange refreshEffectPasses
        ..add(visState.params.effects, 'dotScreen').onFinishChange refreshEffectPasses
        ..add(visState.params.effects, 'glitch').onFinishChange refreshEffectPasses

    for seq in visState.seqs
      folder = visStateGui.addFolder(seq.constructor.displayName)
      for k,v of seq.params
        folder.add(seq.params, k)

    # Listeners

    window.addEventListener \resize, onWindowResize,10 false
    window.addEventListener \keypress, onKeyPress, false

    # updateOptions!
  # <<init

  onWindowResize = !->
    fetchCanvasSize!

    if perspCamActive isnt visState.perspCam
      if visState.perspCam
        camera := with (new THREE.PerspectiveCamera 24, canvasRatio, 10, 50)
          ..position
            ..x = 17
            ..z = 15.0
          ..rotation.y = 0.9
      else
        camera := let p = cameraParams
          new THREE.OrthographicCamera p.left, p.right, p.top, p.bottom, p.near, p.far
      renderPass.camera = camera
      perspCamActive := visState.perspCam

    # camera.aspect = canvasRatio
    if not perspCamActive
      camera <<< cameraParams
      camera.updateProjectionMatrix!

    renderer.setSize canvasWidth, canvasHeight
    composer.setSize canvasWidth * dpr * sampleRatio, canvasHeight * dpr * sampleRatio
  # <<onWindowResize

  onKeyPress = !(e) ->
    | e.keyCode == 32 => # Space
      visState.running = not visState.running
      e.preventDefault!
    | e.keyCode == 84 || e.keyCode == 116 => # T
      console.log \TTT
      e.preventDefault!
    | e.keyCode == 97 =>
      visState.scheduleSequence (mod visState.activeSeqIndex-1, visState.seqs.length)
      e.preventDefault!
    | e.keyCode == 115 =>
      visState.scheduleSequence (mod visState.activeSeqIndex+1, visState.seqs.length)
      e.preventDefault!
    # | _               => console.log \OtherKey, e.keyCode
    # | otherwise => console.log e.keyCode

  rebuildTris = !->
    scene.remove object if object?

    object := new THREE.Object3D()
    # object.frustumCulled = false
    # object.doubleSided = true
    scene.add object

    visState.clearTris!
    # for coord, mesh of Vis.triMeshes!
    #   [cx, cy] = coord
    Vis.triMeshes (mesh, cx, cy) ->
      object.add mesh
      visState.addTri mesh, cx, cy

  # refreshEffectPasses = !->
  #   lastActivePass = void

  #   for name in passesOrder
  #     pass = passes[name]
  #     pass.enabled = visState.params.effects[name]
  #     pass.renderToScreen = false

  #     lastActivePass = pass if pass.enabled

  #   lastActivePass.renderToScreen = true if lastActivePass
  #   effectCopy.enabled = lastActivePass is void

  # HACKY atm
  refreshEffectPasses = !->
    lastActivePass = void

    for name in passesOrder
      pass = passes[name]
      pass.enabled = visState.params.effects[name]
      pass.renderToScreen = false

      lastActivePass = name if pass.enabled

    effectCopy.renderToScreen = lastActivePass == \bloom || lastActivePass is void
    passes[lastActivePass].renderToScreen = true if lastActivePass
    # effectCopy.enabled = lastActivePass is void
    effectCopy.enabled = true


  # Main loop

  tLastStep = 0

  function step(tNow)
    stats.begin!
    
    requestAnimationFrame( step )

    # object.rotation.x += 0.005
    # object.rotation.y += 0.01

    tNow ||= 0
    dt = tNow - tLastStep
    visState.step dt
    tLastStep := tNow

    renderer.clear() #???
    composer.render!
    
    stats.end!
  # <<step

  init!
  step!
# <<main

main!