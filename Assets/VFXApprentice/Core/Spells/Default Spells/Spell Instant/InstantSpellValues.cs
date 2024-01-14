using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class InstantSpellValues
{
    [Tooltip("Shown in the UI when the spell is selected")]
    public string spellName = "New Instant Spell";
    public GameObject windUpVFX;
    [Tooltip("The transform at which the windUpVFX will spawn")]
    public Transform windUpVFXSocket;
    public GameObject castVFX;
    public GameObject areaVFX;
    public GameObject impactVFX;
    public float impactCollisionRadius;
    [Tooltip("The intensity with which dummies or destructibles react to the spell")]
    public float damageIntensity = 1;
}
