using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SpellcasterComponent : MonoBehaviour
{
    public bool useSpellsFromCreateSpellsHere = true;

    [Header("Default Spells Variations")]
    [NonReorderable]
    public List<RangedSpellValues> spellbookRanged;
    [NonReorderable]
    public List<MeleeSpellValues> spellbookMelee;
    [NonReorderable]
    public List<InstantSpellValues> spellbookInstant;

    [Header("Custom Spells")]
    [NonReorderable]
    public List<Spell> spellbook;

    [Header("Debug")]
    public Spell currentlyPlayingSpell;
    public VFXACharacter character;
    public int selectedSpellIndex;
    public ESpellType selectedSpellType;

    private InterfaceManager ui;

    //UNITY METHODS
    void Start() {
        //Get the character component from the same gameobject
        this.character = this.gameObject.GetComponent<VFXACharacter>();
        //Load Spells from the "Create Spells" Gameobject (if exists)
        if (useSpellsFromCreateSpellsHere) {
            CreateSpellsHere csh = GameObject.FindObjectOfType<CreateSpellsHere>();
            if (csh != null) {
                spellbookRanged = csh.spellbookRanged;
                spellbookMelee = csh.spellbookMelee;
                spellbookInstant = csh.spellbookInstant;
                spellbook = csh.spellbook;
            }
        }
        //Get the InterfaceManager reference and update interface
        ui = FindObjectOfType<InterfaceManager>();
        UpdateSpellUI();
    }

    //Spell selection
    public void SetSpellType(ESpellType type) {
        selectedSpellType = type;
        selectedSpellIndex = 0;
        UpdateSpellUI();
    }
    public void CycleNextSpell() {
        selectedSpellIndex++;
        ClampSelectedSpellIndex();
        UpdateSpellUI();
    }
    private void ClampSelectedSpellIndex() {
        int count = 0;
        //TODO - Switch expression?
        switch (selectedSpellType) {
            case ESpellType.Custom:
                count = spellbook.Count;
                break;
            case ESpellType.Ranged:
                count = spellbookRanged.Count;
                break;
            case ESpellType.Melee:
                count = spellbookMelee.Count;
                break;
            case ESpellType.Instant:
                count = spellbookInstant.Count;
                break;
            default:
                break;
        }
        //If the correct spellbook has spells
        if (count > 0) {
            selectedSpellIndex %= count;
        } else {
            selectedSpellIndex = 0;
        }
    }
    private void UpdateSpellUI() {
        if(selectedSpellType == ESpellType.Custom) {
            ui.SetSelectedSpell(spellbook[selectedSpellIndex]);
            ui.SetSpellIndex(selectedSpellIndex, spellbook.Count);
        } else {
            switch (selectedSpellType) {
                case ESpellType.Ranged:
                    ui.SetSelectedSpell(selectedSpellType, spellbookRanged[selectedSpellIndex].spellName);
                    ui.SetSpellIndex(selectedSpellIndex, spellbookRanged.Count);
                    break;
                case ESpellType.Melee:
                    ui.SetSelectedSpell(selectedSpellType, spellbookMelee[selectedSpellIndex].spellName);
                    ui.SetSpellIndex(selectedSpellIndex, spellbookMelee.Count);
                    break;
                case ESpellType.Instant:
                    ui.SetSelectedSpell(selectedSpellType, spellbookInstant[selectedSpellIndex].spellName);
                    ui.SetSpellIndex(selectedSpellIndex, spellbookInstant.Count);
                    break;
                default:
                    break;
            }
        }
    }
    //Cast Spell
    public void CastSpell() {
        //Check if the character can cast a spell right now
        if (GetCanCastSpell()) {
            //Spawn the selected spell gameobject
            SpawnSpell();
            //If the spell has ben created
            if(currentlyPlayingSpell != null) {
                //Tell the spell to start
                currentlyPlayingSpell.Cast();
            }
        }
    }
    //Spawn Spell GameObject
    private bool SpawnSpell() {
        return selectedSpellType switch {
            ESpellType.Custom => SpawnSpellCustom(),
            ESpellType.Ranged => SpawnSpellRanged(),
            ESpellType.Melee => SpawnSpellMelee(),
            ESpellType.Instant => SpawnSpellInstant(),
            _ => false,
        };
    }
    private bool SpawnSpellRanged() {
        if (spellbookRanged.Count == 0) {
            return false;
        }
        //Spawn Spell
        GameObject spellGameObject = new GameObject("New Ranged Spell");
        spellGameObject.transform.position = character.transform.position;
        spellGameObject.transform.rotation = character.transform.rotation;
        RangedSpell spell = spellGameObject.AddComponent<RangedSpell>();
        spell.owner = character;
        //Load the values from the Ranged Spellbook
        spell.LoadRangedSpellValues(spellbookRanged[selectedSpellIndex]);
        //Set this as the currently playing spell
        currentlyPlayingSpell = spell;
        return true;
    }
    private bool SpawnSpellMelee() {
        if(spellbookMelee.Count == 0) {
            return false;
        }
        //Spawn Spell
        GameObject spellGameObject = new GameObject("New Melee Spell");
        spellGameObject.transform.position = character.transform.position;
        spellGameObject.transform.rotation = character.transform.rotation;
        MeleeSpell spell = spellGameObject.AddComponent<MeleeSpell>();
        spell.owner = character;
        //Load the values from the Melee Spellbook
        spell.LoadMeleeSpellValues(spellbookMelee[selectedSpellIndex]);
        //Set this as the currently playing spell
        currentlyPlayingSpell = spell;
        return true;
    }
    private bool SpawnSpellInstant() {
        if (spellbookInstant.Count == 0) {
            return false;
        }
        //Spawn Spell
        GameObject spellGameObject = new GameObject("New Instant Spell");
        spellGameObject.transform.position = character.transform.position;
        spellGameObject.transform.rotation = character.transform.rotation;
        InstantSpell spell = spellGameObject.AddComponent<InstantSpell>();
        spell.owner = character;
        //Load the values from the Instant Spellbook
        spell.LoadInstantSpellValues(spellbookInstant[selectedSpellIndex]);
        //Set this as the currently playing spell
        currentlyPlayingSpell = spell;
        return true;
    }
    private bool SpawnSpellCustom() {
        //TODO
        GameObject spellGO = spellbook[selectedSpellIndex].gameObject;
        spellGO = Instantiate(spellGO);
        Spell newSpell = spellGO.GetComponent<Spell>();
        newSpell.owner = character;
        currentlyPlayingSpell = newSpell;
        return true;
    }
    //Release Spell
    public void ReleaseSpell() {
        if (currentlyPlayingSpell != null && !currentlyPlayingSpell.bSpellReleased) {
            currentlyPlayingSpell.Release();
        }
    }

    //UTILS
    private bool GetCanCastSpell() {
        return (character.isGrounded && currentlyPlayingSpell == null);
    }
}
