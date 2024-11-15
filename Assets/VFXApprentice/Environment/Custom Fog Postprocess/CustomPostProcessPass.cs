using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

public class CustomPostProcessPass : ScriptableRenderPass
{   
    // Used to render from camera to post processings
    // back and forth, until we render the final image to
    // the camera
    RenderTargetIdentifier source;
    RenderTargetIdentifier destinationA;
    RenderTargetIdentifier destinationB;
    RenderTargetIdentifier latestDest;

    readonly int temporaryRTIdA = Shader.PropertyToID("_TempRT");
    readonly int temporaryRTIdB = Shader.PropertyToID("_TempRTB");

    public CustomPostProcessPass() {
        // Set the render pass event
        renderPassEvent = RenderPassEvent.BeforeRenderingPostProcessing;
    }

    public override void OnCameraSetup(CommandBuffer cmd, ref RenderingData renderingData) {
        // Grab the camera target descriptor. We will use this when creating a temporary render texture.
        RenderTextureDescriptor descriptor = renderingData.cameraData.cameraTargetDescriptor;
        descriptor.depthBufferBits = 0;

        var renderer = renderingData.cameraData.renderer;
        source = renderer.cameraColorTarget;

        // Create a temporary render texture using the descriptor from above.
        cmd.GetTemporaryRT(temporaryRTIdA, descriptor, FilterMode.Bilinear);
        destinationA = new RenderTargetIdentifier(temporaryRTIdA);
        cmd.GetTemporaryRT(temporaryRTIdB, descriptor, FilterMode.Bilinear);
        destinationB = new RenderTargetIdentifier(temporaryRTIdB);
    }

    // The actual execution of the pass. This is where custom rendering occurs.
    public override void Execute(ScriptableRenderContext context, ref RenderingData renderingData) {
        // Skipping post processing rendering inside the scene View
        if (renderingData.cameraData.isSceneViewCamera) {
            //return;
        }

        // Here you get your materials from your custom class
        // (It's up to you! But here is how I did it)
        var materials = CustomPostProcessingMaterials.Instance;
        if (materials == null) {
            Debug.LogError("Custom Post Processing Materials instance is null");
            return;
        }

        CommandBuffer cmd = CommandBufferPool.Get("Custom Post Processing");
        cmd.Clear();

        // This holds all the current Volumes information
		// which we will need later
        var stack = VolumeManager.instance.stack;

        #region Local Methods

        // Swaps render destinations back and forth, so that
        // we can have multiple passes and similar with only a few textures
        void BlitTo(Material mat, int pass = 0) {
            var first = latestDest;
            var last = first == destinationA ? destinationB : destinationA;
            Blit(cmd, first, last, mat, pass);

            latestDest = last; 
        }

        #endregion

        // Starts with the camera source
        latestDest = source;

        //---Custom effect here---
        var vfxaFog = stack.GetComponent<VFXAFogComponent>();
        // Only process if the effect is active
        if (vfxaFog.IsActive()) {
            var material = materials.VFXAFogMaterial;
            // P.s. optimize by caching the property ID somewhere else
            material.SetFloat(Shader.PropertyToID("_Intensity"), vfxaFog.intensity.value);
            material.SetColor(Shader.PropertyToID("_FogColor"), vfxaFog.fogColor.value);
            material.SetFloat(Shader.PropertyToID("_FogDistance"), vfxaFog.fogDistance.value);
            material.SetFloat(Shader.PropertyToID("_FogHeight"), vfxaFog.fogHeight.value);
            material.SetFloat(Shader.PropertyToID("_FogExponent"), vfxaFog.fogExponent.value);
            material.SetFloat(Shader.PropertyToID("_NoiseScale"), vfxaFog.noiseScale.value);
            material.SetFloat(Shader.PropertyToID("_NoiseStrength"), vfxaFog.noiseStrength.value);
            material.SetFloat(Shader.PropertyToID("_SkyboxDistance"), vfxaFog.skyboxStartDistance.value);
            material.SetFloat(Shader.PropertyToID("_SkyboxFogIntensity"), vfxaFog.skyboxFogIntensity.value);

            BlitTo(material);
        }

        
        var vfxaFogVolumetric = stack.GetComponent<VFXAVolumetricFog>();
        // Only process if the effect is active
        if (vfxaFogVolumetric.IsActive()) {
            var material = materials.VFXAVolumetricFogMaterial;
            // P.s. optimize by caching the property ID somewhere else
            material.SetFloat(Shader.PropertyToID("_Intensity"), vfxaFogVolumetric.intensity.value);

            BlitTo(material);
        }

        // Add any other custom effect/component you want, in your preferred order
        // Custom effect 2, 3 , ...


        // DONE! Now that we have processed all our custom effects, applies the final result to camera
        Blit(cmd, latestDest, source);

        context.ExecuteCommandBuffer(cmd);
        CommandBufferPool.Release(cmd);
    }

    //Cleans the temporary RTs when we don't need them anymore
    public override void OnCameraCleanup(CommandBuffer cmd) {
        cmd.ReleaseTemporaryRT(temporaryRTIdA);
        cmd.ReleaseTemporaryRT(temporaryRTIdB);
    }
}
