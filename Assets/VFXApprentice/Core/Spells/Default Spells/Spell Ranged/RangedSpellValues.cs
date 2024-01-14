using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class RangedSpellValues
{
    [Tooltip("Shown in the UI when the spell is selected")]
    public string spellName = "New Ranged Spell";
    public GameObject windUpVFX;
    public Transform windUpVFXSocket;
    public GameObject castVFX;
    public GameObject projectileGameObject;
}
