# Philips Hue API wrapper

[![GitHub CI](https://github.com/gdziadkiewicz/philips-hue-haskell/workflows/CI/badge.svg)](https://github.com/gdziadkiewicz/philips-hue-haskell/actions)

Control Philips Hue lights with Haskell.

To get started, import the `Hue` module.

API Coverage
------------
- [x] **Lights**
  - [x] Get all lights
  - [x] Get new lights
  - [x] Search for lights
  - [x] Get light attributes/state
  - [x] Rename lights
  - [x] Set light state
  - [x] Delete light
- [x] **Groups**
  - [x] Get all groups
  - [x] Create group
  - [x] Get group attributes
  - [x] Set group attributes
  - [x] Set group state
  - [x] Delete group
- [ ] **Schedules**
- [ ] **Scenes**
- [ ] **Sensors**
  - [x] Get all sensors
  - [x] Get new sensors
  - [x] Search for sensor
  - [ ] Get sensor attributes/state/config
  - [ ] Set sensor attributes/state/config
  - [x] Rename sensor
  - [x] Delete sensor
- [ ] **Rules**
- [ ] **Configuration**
- [ ] **Resourcelinks**
- [ ] **Capabilities**

Running `Hue` actions
---------------------

```haskell
runHue $ do
  ls <- lights
  traverse (setLight on) ls
```

Examples
--------

Slowly change a lights' color:
```haskell
colorShift = do
  Just l <- lightWithName "light-on-my-desk"
  forever $ do
    setLight (on <> increaseHue 1000 <> increaseSaturation 32) l 
    liftIO $ threadDelay 500000
```

Change the state of multiple lights:
```haskell
allLightsOn = lights >>= traverse (setLight on)
```

```haskell
someLightsOn = do
  ls <- catMaybes <$> traverse lightWithName ["Desk", "Couch", "Kitchen"]
  traverse (setLight (on <> brightness 100)) ls 
```

Change the display name:
```haskell
renameTo name newName = do
  Just l <- lightWithName name
  renameLight l (LightName newName)
```

Perform an `alert`:
```haskell
doAlert name = do
  Just l <- lightWithName name
  setLight (alert MultipleCycles) l 
```

Bridge discovery and configuration
----------------------------------
Interactive auto discovery is provided by `Hue.Config`.

Example with GHCi:

```
> stack ghci
λ import Hue.Config
λ let conf = getHueConfig
λ configIP <$> conf
It seems like there is no configuration present.
Searching for bridge, this may take a minute...
Found bridge with id: 0017881717c1
Push the button on your bridge.
BridgeIP {ipAddress = "192.168.1.100"}
λ 
```
