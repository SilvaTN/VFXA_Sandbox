using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

public class VFXA_Particles_VertexStream_CustomEditor : ShaderGUI
{
    bool showTextureOne = true;
    bool showTextureTwo = true;
    bool showMask = true;

    override public void OnGUI(MaterialEditor materialEditor, MaterialProperty[] properties)
    {
        EditorGUILayout.HelpBox("Values driven by the Particle System \n\n "
            + "    CUSTOM 1 \n" +
            "--------------------- \n" +
            "   X: Main tiling X    \n  " +
            " Y: Main tiling Y  \n" +
            "   Z: Main offset X  \n" +
            "   W: Main offset Y  \n\n" +
            "      CUSTOM 2 \n" +
            "--------------------  \n" +
            "   X: Mask tiling X \n " +
            "  Y: Mask tiling Y \n" +
            "   Z: Mask offset X \n" +
            "   W: Mask offset Y \n" +
            "", MessageType.Info);

        base.OnGUI(materialEditor, properties);
    }
}
