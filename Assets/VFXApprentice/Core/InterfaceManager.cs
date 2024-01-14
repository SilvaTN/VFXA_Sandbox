using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class InterfaceManager : MonoBehaviour
{
    [Header("Cursor")]
    public Texture2D cursorTexture;

    [Header("Spell Panel")]
    public Image spellIcon;
    public Sprite rangedSpellIcon;
    public Sprite meleeSpellIcon;
    public Sprite instantSpellIcon;
    public List<Image> spellTypeSelectors;
    public TMPro.TextMeshProUGUI spellName;
    public TMPro.TextMeshProUGUI selectedSpellIndex;

    [Header("UI Visibility")]
    public List<GameObject> visibleUIElements;
    public List<GameObject> hiddenUIElements;
    public bool isVisible = true;

    private void Start() {
        SetUIVisibility(true);
        Cursor.SetCursor(cursorTexture, new(19,10), CursorMode.ForceSoftware);
    }

    public void SetSelectedSpell(Spell s) {
        spellIcon.sprite = s.spellIcon;
        spellName.text = s.spellName;
        for (int i = 0; i < spellTypeSelectors.Count; i++) {
            spellTypeSelectors[i].enabled = false;
        }
    }

    public void SetSelectedSpell(ESpellType type, string name) {
        //Spell Icon
        spellIcon.sprite = type switch {
            ESpellType.Ranged => rangedSpellIcon,
            ESpellType.Melee => meleeSpellIcon,
            ESpellType.Instant => instantSpellIcon,
            _ => null,
        };

        //Spell Name
        spellName.text = name;

        //Spell Type selector
        for(int i=0; i < spellTypeSelectors.Count; i++) {
            if(i == (int)type-1) {
                spellTypeSelectors[i].enabled = true;
            } else {
                spellTypeSelectors[i].enabled = false;
            }
        }
    }

    public void SetSpellIndex(int index, int count) {
        selectedSpellIndex.text = (index+1).ToString() + "/" + count.ToString();
    }

    public void SetUIVisibility(bool isVisible) {
        this.isVisible = isVisible;
        foreach (GameObject g in visibleUIElements) {
            g.SetActive(isVisible);
        }
        foreach (GameObject g in hiddenUIElements) {
            g.SetActive(!isVisible);
        }
    }
}
