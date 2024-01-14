using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GameManager : Singleton<GameManager>
{

    public int targetFPS;

    [HideInInspector]
    public VFXACharacter character;
    [HideInInspector]
    public InterfaceManager ui;

    new private void Awake() {
        base.Awake();
        character = GameObject.FindObjectOfType<VFXACharacter>();
        ui = GameObject.FindObjectOfType<InterfaceManager>();

        Application.targetFrameRate = targetFPS;
    }
}
