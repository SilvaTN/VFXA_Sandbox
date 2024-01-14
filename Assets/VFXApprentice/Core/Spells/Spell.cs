using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public enum ESpellType { Custom, Ranged, Melee, Instant, Count, Null };

[System.Serializable]
public class Spell : MonoBehaviour
{
    public string spellName;
    [SerializeField]
    public Sprite spellIcon;
    public VFXACharacter owner;
    public bool bSpellCast;
    public bool bSpellReleased;

    //UNITY METHODS
    public void Start() {
    }
    public void Update() {
    }

    //SPELL EVENTS
    public virtual void Cast() {
        bSpellCast = true;
    }
    public virtual void Release() {
        bSpellReleased = true;
    }
    public virtual void CustomSpellEvent(string code) {
        //print("CustomSpellEvent " + code);
    }
}
