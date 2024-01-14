using System;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

[Serializable, VolumeComponentMenuForRenderPipeline("Custom/VFXA Volumetric Fog", typeof(UniversalRenderPipeline))]
public class VFXAVolumetricFog : VolumeComponent, IPostProcessComponent
{

    public ClampedFloatParameter intensity = new ClampedFloatParameter(value: 0, min: 0, max: 1, overrideState: true);
    public bool IsActive() => intensity.value > 0;
    public bool IsTileCompatible() => true;
}