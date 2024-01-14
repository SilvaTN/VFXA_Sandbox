using System;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

[Serializable, VolumeComponentMenuForRenderPipeline("Custom/VFXA Fog", typeof(UniversalRenderPipeline))]
public class VFXAFogComponent : VolumeComponent, IPostProcessComponent
{
    [Header("Fog")]
    public ClampedFloatParameter intensity = new ClampedFloatParameter(value: 0, min: 0, max: 1, overrideState: true);
    public NoInterpColorParameter fogColor = new NoInterpColorParameter(Color.white);
    public FloatParameter fogDistance = new FloatParameter(value:150, overrideState: true);
    public ClampedFloatParameter fogHeight = new ClampedFloatParameter(value: 100, min: 0, max: 200, overrideState: true);
    public FloatParameter fogExponent = new FloatParameter(value:1, overrideState: true);
    public FloatParameter noiseScale = new FloatParameter(value:500, overrideState: true);
    public FloatParameter noiseStrength = new FloatParameter(value:0.25f, overrideState: true);
    
    [Header("Skybox Fog")]
    public FloatParameter skyboxStartDistance = new FloatParameter(value: 500, overrideState: true);
    public ClampedFloatParameter skyboxFogIntensity = new ClampedFloatParameter(value: 1f, min: 0, max: 1, overrideState: true);
    public bool IsActive() => intensity.value > 0;
    public bool IsTileCompatible() => true;
}
