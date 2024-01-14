using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CreateSpellsHere : MonoBehaviour
{
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
}
