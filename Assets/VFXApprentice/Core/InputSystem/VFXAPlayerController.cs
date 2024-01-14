using System;
using UnityEngine;
using UnityEngine.InputSystem;

public class VFXAPlayerController : MonoBehaviour
{
    [Header("Character Input")]
    public Vector2 movement;
    public Vector2 mouseDelta;
    public Vector2 mousePosition;
    public bool jump;
    public bool attack;
    public bool sprint;
    public bool dragCamera;
    public float cameraZoom;

    [Header("Cursor Settings")]
    public bool cursorLocked = true;

    public VFXACharacter character;
    private Keyboard keyboard;
    private Mouse mouse;
    private InterfaceManager ui;

    private float timescale;

    //INPUT WORKAROUND
    private bool shiftKey;
    private bool altKey;

    //UNITY METHODS
    private void OnApplicationFocus(bool hasFocus) {
        SetCursorLock(cursorLocked);
    }
    private void Start() {
        //Get references
        keyboard = InputSystem.GetDevice<Keyboard>();
        mouse = InputSystem.GetDevice<Mouse>();
        character = GameManager.Instance.character;
        ui = GameManager.Instance.ui;
        timescale = 1;
    }

    //INPUT HANDLING - Spells & Interface
    private void Update() {
        //Update key modifiers
        shiftKey = keyboard.shiftKey.isPressed;
        altKey = keyboard.altKey.isPressed;


        if (keyboard.digit1Key.wasPressedThisFrame) {
            character.spellcaster.SetSpellType(ESpellType.Ranged);
        }
        if (keyboard.digit2Key.wasPressedThisFrame) {
            character.spellcaster.SetSpellType(ESpellType.Melee);
        }
        if (keyboard.digit3Key.wasPressedThisFrame) {
            character.spellcaster.SetSpellType(ESpellType.Instant);
        }
        if (keyboard.digit4Key.wasPressedThisFrame) {
            character.spellcaster.SetSpellType(ESpellType.Custom);
        }
        //TAB - Cycle Spell
        if (keyboard.tabKey.wasPressedThisFrame) {
            character.spellcaster.CycleNextSpell();
        }
        //LMB Press - Start Spell
        if (mouse.leftButton.wasPressedThisFrame) {
            character.CastSpell();
        }
        //LMB Release - Release Spell
        if (mouse.leftButton.wasReleasedThisFrame) {
            character.ReleaseSPell();
        }
        //HACK - handling UI visibility through here since the new input system
        //doesn't appear to handle modifiers well
        if(keyboard.hKey.wasPressedThisFrame && !keyboard.shiftKey.isPressed && !keyboard.altKey.isPressed) {
            ui.SetUIVisibility(!ui.isVisible);
        }

    }

    //INPUT HANDLING - Character
    public void OnMove(InputValue value) {
        movement = value.Get<Vector2>();
    }
    public void OnJump(InputValue value) {
        jump = value.isPressed;
    }
    public void OnSprint(InputValue value) {
        sprint = value.isPressed;
    }
    
    //INPUT HANDLING - Camera
    public void OnDraggingCamera(InputValue value) {
        dragCamera = value.Get<float>() != 0f;
        if (dragCamera) {
            // What to do when the camera starts getting dragged
            cursorLocked = true;
            SetCursorVisibility(false);
        } else {
            cursorLocked = false;
            SetCursorVisibility(true);
            mouseDelta = Vector2.zero;
        }
        SetCursorLock(cursorLocked);
    }
    public void OnLook(InputValue value) {
        if (dragCamera) {
            mouseDelta = value.Get<Vector2>();
        }
    }
    public void OnMousePosition(InputValue value) {
        mousePosition = value.Get<Vector2>();
    }
    public void OnCameraZoom(InputValue value) {
        float v = value.Get<Vector2>().y;
        if (shiftKey) {
            //Timescale
            if (v > 0) {
                IncreaseTimescale();
            } else if (v < 0) {
                DecreaseTimescale();
            }
        } else {
            //Camerazoom
            if (v > 0) {
                cameraZoom = -1;
            } else if (v < 0) {
                cameraZoom = 1;
            } else {
                cameraZoom = 0;
            }
        }
    }

    //INPUT HANDLING - Time Scale
    public void OnTimeScaleReset(InputValue value) {
        ResetTimescale();
    }
    public void IncreaseTimescale() {
        timescale += 0.1f;
        timescale = Mathf.Clamp(timescale, 0.1f, 2f);
        Time.timeScale = timescale;
    }
    public void DecreaseTimescale() {
        timescale -= 0.1f;
        timescale = Mathf.Clamp(timescale, 0.1f, 2f);
        Time.timeScale = timescale;
    }
    public void ResetTimescale() {
        timescale = 1;
        Time.timeScale = timescale;
    }

    //INPUT HANDLING - UI Visibility
    public void OnToggleUIVisibility(InputValue value) {
        ui.SetUIVisibility(!ui.isVisible);
    }
    public void OnToggleCharacterVisibility(InputValue value) {
        character.ToggleCharacterVisibility();
    }
    public void OnToggleCursorVisibility(InputValue value) {
        ToggleCursorVisibility();
    }

    //UTILS
    public void SetCursorVisibility(bool visibility) {
        Cursor.visible = visibility;
    }
    public void ToggleCursorVisibility() {
        Cursor.visible = !Cursor.visible;
    }

    private void SetCursorLock(bool newState) {
        Cursor.lockState = newState ? CursorLockMode.Locked : CursorLockMode.None;
    }
}