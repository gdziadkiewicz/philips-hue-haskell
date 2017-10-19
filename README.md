# Philips Hue API wrapper

Control Philips Hue lights with Haskell.

To get started, import the `Hue` module.

Running `Hue` actions
---------------------

```haskell
runHue $ do
  ls <- fetchLights
  traverse (request . setLight on) ls
```

Examples
--------

Slowly change a lights' color:
```haskell
colorShift = do
  Just l <- lightWithName "light-on-my-desk"
  forever $ do
    request $ setLight (on <> increaseHue 1000 <> increaseSaturation 32) l 
    liftIO $ threadDelay 500000
```

Change the state of multiple lights:
```haskell
allLightsOn = fetchLights >>= traverse (request . setLight on)
```

```haskell
someLightsOn = do
  ls <- catMaybes <$> traverse lightWithName ["Desk", "Couch", "Kitchen"]
  traverse (request . setLight (on <> brightness 100)) ls 
```

Change the display name:
```haskell
renameTo name newName = do
  Just l <- lightWithName name
  request $ renameLight l (LightName newName)
```

Perform an `alert`:
```haskell
doAlert name = do
  Just l <- lightWithName name
  request $ setLight (alert MultipleCycles) l 
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
