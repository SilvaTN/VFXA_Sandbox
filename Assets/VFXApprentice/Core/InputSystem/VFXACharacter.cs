using UnityEngine;
using UnityEngine.InputSystem;
using Cinemachine;

/* Note: animations are called via the controller for both the character and capsule using animator null checks
 */

[RequireComponent(typeof(CharacterController))]
[RequireComponent(typeof(PlayerInput))]
public class VFXACharacter : MonoBehaviour
{
    //PARAMETERS
    #region
    [Header("Char acter Configuration")]
    [Tooltip("Move speed of the character in m/s")]
    public float regularSpeed = 2.0f;
    [Tooltip("Sprint speed of the character in m/s")]
    public float sprintingSpeed = 5.335f;
    public float acceleration = 10.0f;
    [Tooltip("How fast the character turns to face movement direction")]
    [Range(0.0f, 0.3f)]
    public float rotationSmoothing = 0.12f;

    [Space(10)]
    public AudioClip LandingAudioClip;
    public AudioClip[] FootstepAudioClips;
    [Range(0, 1)] public float FootstepAudioVolume = 0.5f;

    [Space(10)]
    [Tooltip("The height the player can jump")]
    public float JumpHeight = 1.2f;
    [Tooltip("The character uses its own gravity value. The engine default is -9.81f")]
    public float Gravity = -15.0f;
    [Tooltip("Maximum velocity the character can achieve when falling")]
    public float TerminalVelocity = -53.0f;
    [Tooltip("Time required to pass before being able to jump again after landing. Set to 0f to instantly jump again")]
    public float JumpTimeout = 0.50f;

    [Space(10)]
    [Tooltip("Useful for rough ground")]
    public float GroundedOffset = -0.14f;
    [Tooltip("The radius of the grounded check. Should match the radius of the CharacterController")]
    public float GroundedRadius = 0.28f;
    [Tooltip("What layers the character uses as ground")]
    public LayerMask GroundLayers;
    #endregion


    [Header("Cinemachine")]
    [Tooltip("The follow target set in the Cinemachine Virtual Camera that the camera will follow")]
    public GameObject cmCameraTarget;
    public CinemachineVirtualCamera cmVirtualCamera;
    [Tooltip("How far in degrees can you move the camera up")]
    public float maxPitch = 70.0f;
    [Tooltip("How far in degrees can you move the camera down")]
    public float minPitch = 15.0f;
    [Tooltip("The pitch of the camera when the game starts")]
    public float startingCameraPitch = 30f;
    [Tooltip("Additional degress to override the camera. Useful for fine tuning camera position when locked")]
    public float CameraAngleOverride = 0.0f;
    public float maxCameraDistance = 25f;
    public float minCameraDistance = 5f;
    [Tooltip("For locking the camera position on all axis")]
    public bool LockCameraPosition = false;

    //REFERENCES
    private PlayerInput playerInput;
    public Animator animator;
    private CharacterController characterController;
    private VFXAPlayerController input;
    private GameObject cam;
    public SpellcasterComponent spellcaster;
    public GameObject characterMesh;

    //State

    //Movement - Horizontal
    public bool canMove = true;
    public bool canRotate = true;
    private float currentSpeed;
    private Vector3 currentDirection;
    private float _animationBlend;

    //Movement - Vertical
    public bool isGrounded = true;
    private float verticalVelocity;
    private float jumpTimeoutTimer;

    //Abilities
    private bool isChargingAttack;
    private bool isAttacking;
    private bool isReleasingAttack;

    //Rotation
    private float currentRotationVelocity;

    //Camera
    private float cameraYaw;
    private float cameraPitch;
    private float targetCameraDistance;


    private const float lookDeadzone = 0.01f;

    private bool usingMouse {
        get {
            return playerInput.currentControlScheme == "KeyboardMouse";
        }
    }

    //UNITY EVENTS
    private void Awake() {
        // get a reference to our main camera
        if (cam == null) {
            cam = Camera.main.gameObject;
        }
    }
    private void Start() {
        //Load Components
        animator = GetComponent<Animator>();
        characterController = GetComponent<CharacterController>();
        input = GetComponent<VFXAPlayerController>();
        playerInput = GetComponent<PlayerInput>();
        spellcaster = GetComponent<SpellcasterComponent>();

        //Set the initial camera pitch and yaw
        cameraYaw = cmCameraTarget.transform.rotation.eulerAngles.y;
        cameraPitch = startingCameraPitch;
        targetCameraDistance = 15;

        // reset our timeouts on start
        jumpTimeoutTimer = JumpTimeout;
    }
    private void Update() {
        //Check if the character is grounded and write to isGrounded
        UpdateGroundCheck();
        //Compute and apply the character displacement and rotation according to the input
        UpdateMovement();
        //Attack if the inputs calls for it and we are ground
        if (input.attack && isGrounded) {
            input.attack = false;
            Attack();
        }
        //Attack Animations
        UpdateAttackStates();
    }
    private void LateUpdate() {
        CameraRotation();
        CameraZoom(input.cameraZoom);
    }
    private void OnDrawGizmosSelected() {
        Color transparentGreen = new Color(0.0f, 1.0f, 0.0f, 0.35f);
        Color transparentRed = new Color(1.0f, 0.0f, 0.0f, 0.35f);

        if (isGrounded) Gizmos.color = transparentGreen;
        else Gizmos.color = transparentRed;

        // when selected, draw a gizmo in the position of, and matching radius of, the grounded collider
        Gizmos.DrawSphere(
            new Vector3(transform.position.x, transform.position.y - GroundedOffset, transform.position.z),
            GroundedRadius);
    }

    //MOVEMENT
    private void UpdateGroundCheck() {
        //Check Sphere collision at the grounded position
        Vector3 spherePosition = new(transform.position.x, transform.position.y - GroundedOffset, transform.position.z);
        isGrounded = Physics.CheckSphere(spherePosition, GroundedRadius, GroundLayers, QueryTriggerInteraction.Ignore);

        //Update animator parameter
        animator.SetBool("isGrounded", isGrounded);
    }
    private void UpdateMovement() {
        UpdateHorizontalSpeed();
        UpdateHorizontalDirection();
        UpdateVerticalSpeed();

        Vector3 horizontalV = currentDirection.normalized * currentSpeed;
        Vector3 verticalV = new Vector3(0.0f, verticalVelocity, 0.0f);

        //Apply the player movement
        characterController.Move((horizontalV + verticalV) * Time.deltaTime);
    }
    private void UpdateHorizontalSpeed() {
        //Set targetSpeed to the desired speed
        float targetSpeed = input.sprint ? sprintingSpeed : regularSpeed;
        //Note: Vector2's == operator uses approximation so is not floating point error prone, and is cheaper than magnitude
        //If the character is set to canMove=false set the targetSpeed to 0
        if (input.movement == Vector2.zero || !canMove) {
            targetSpeed = 0.0f;
        }
        float currentHorizontalSpeed = new Vector3(characterController.velocity.x, 0.0f, characterController.velocity.z).magnitude;
        float precision = 0.1f;
        float speedDelta = Mathf.Abs(currentHorizontalSpeed - targetSpeed);

        if (speedDelta > precision) { //If the current Horizontal Speed is too different from the target speed
            //Lerp the player speed towards the target speed
            currentSpeed = Mathf.Lerp(currentHorizontalSpeed, targetSpeed, Time.deltaTime * acceleration);
            //round speed to 3 decimal places
            currentSpeed = Mathf.Round(currentSpeed * 1000f) / 1000f;
        } else { //If the current horizontal speed is close enough to the target speed
            //Snap to the target speed
            currentSpeed = targetSpeed;
        }

        //Set the animator parameters
        animator.SetFloat("speed", currentSpeed);
        if (currentSpeed > 2.0f) {
            animator.SetBool("isRunning", true);
            animator.SetBool("isIdling", false);
        } else {
            animator.SetBool("isRunning", false);
            animator.SetBool("isIdling", true);
        }

        //Animation Blend
        _animationBlend = Mathf.Lerp(_animationBlend, targetSpeed, Time.deltaTime * acceleration);
        if (_animationBlend < 0.01f) _animationBlend = 0f;
    }
    private void UpdateHorizontalDirection() {
        Vector3 inputDirection = new Vector3(input.movement.x, 0.0f, input.movement.y).normalized;
        // if there is a move input rotate player when the player is moving
        if (input.movement != Vector2.zero && canRotate) {
            float playerTargetRotation = Mathf.Atan2(inputDirection.x, inputDirection.z) * Mathf.Rad2Deg + cam.transform.eulerAngles.y;
            float rotation = Mathf.SmoothDampAngle(transform.eulerAngles.y, playerTargetRotation, ref currentRotationVelocity, rotationSmoothing);

            // rotate to face input direction relative to camera position
            transform.rotation = Quaternion.Euler(0.0f, rotation, 0.0f);
            currentDirection = Quaternion.Euler(0.0f, playerTargetRotation, 0.0f) * Vector3.forward;
        }

    }
    private void UpdateVerticalSpeed() {
        if (isGrounded) {

            // stop our velocity from dropping infinitely while grounded
            if (verticalVelocity < 0.0f) {
                verticalVelocity = -2f;
            }

            //If the input is telling us to jump and we can jump
            if (input.jump && jumpTimeoutTimer <= 0.0f) {
                //The square root of H * -2 * G = how much velocity needed to reach desired height
                verticalVelocity = Mathf.Sqrt(JumpHeight * -2f * Gravity);
                animator.SetBool("isJumping", true);
            } else {
                animator.SetBool("isJumping", false);
            }

            if (jumpTimeoutTimer >= 0.0f) {
                jumpTimeoutTimer -= Time.deltaTime;
            }
        } else {
            //Keep resetting the jumpTimeout
            jumpTimeoutTimer = JumpTimeout;
            //Ignore jumping inputs while in the air
            input.jump = false;
        }

        // apply gravity over time if under terminal (multiply by delta time twice to linearly speed up over time)
        if (verticalVelocity > TerminalVelocity) {
            verticalVelocity += Gravity * Time.deltaTime;
        }
    }
    private void SetCharacterRotation(Quaternion rotation) {
        this.transform.rotation = rotation;
    }

    public void LookAtCursor() {
        Vector3 lookDirection = MouseToWorld(input.mousePosition) - this.transform.position;
        Quaternion lookRotation = Quaternion.LookRotation(lookDirection.normalized, Vector3.up);
        SetCharacterRotation(lookRotation);
    }
    //TODO needs a different lock for rotation or can use the same without problem?
    public void SetCharacterMobility(bool canMove, bool canRotate) {
        this.canMove = canMove;
        this.canRotate = canRotate;
    }

    //SPELLS
    public void CastSpell() {
        spellcaster.CastSpell();
    }
    public void ReleaseSPell() {
        spellcaster.ReleaseSpell();
    }
    public void CustomSpellEvent(string code) {
        if (spellcaster.currentlyPlayingSpell != null) {
            spellcaster.currentlyPlayingSpell.CustomSpellEvent(code);
        }
    }

    //ANIMATION EVENTS
    private void OnFootstep(AnimationEvent animationEvent) {
        if (animationEvent.animatorClipInfo.weight > 0.5f) {
            if (FootstepAudioClips.Length > 0) {
                var index = Random.Range(0, FootstepAudioClips.Length);
                AudioSource.PlayClipAtPoint(FootstepAudioClips[index], transform.TransformPoint(characterController.center), FootstepAudioVolume);
            }
        }
    }
    private void OnLand(AnimationEvent animationEvent) {
        if (animationEvent.animatorClipInfo.weight > 0.5f) {
            AudioSource.PlayClipAtPoint(LandingAudioClip, transform.TransformPoint(characterController.center), FootstepAudioVolume);
        }
    }

    //CAMERA
    public void CameraZoom(float amount) {
        //Check if the input has chenged the target camera zoom
        targetCameraDistance += amount;
        targetCameraDistance = Mathf.Clamp(targetCameraDistance, minCameraDistance, maxCameraDistance);
        //Set the camera distance smoothly
        Cinemachine3rdPersonFollow cm3PFollow = cmVirtualCamera.GetCinemachineComponent<Cinemachine3rdPersonFollow>();
        if (cm3PFollow != null) {
            cm3PFollow.CameraDistance = Mathf.Lerp(cm3PFollow.CameraDistance, targetCameraDistance, 7.5f * Time.deltaTime);
        }
    }
    private void CameraRotation() {
        // if there is an input and camera position is not fixed
        if (input.mouseDelta.sqrMagnitude >= lookDeadzone && !LockCameraPosition) {
            float yawDelta = input.mouseDelta.x;
            float pitchDelta = input.mouseDelta.y;
            if (!usingMouse) {
                yawDelta *= Time.deltaTime;
                pitchDelta *= Time.deltaTime;
            }
            cameraYaw += yawDelta;
            cameraPitch += pitchDelta;
        }

        // clamp our rotations so our values are limited 360 degrees
        cameraYaw = ClampAngle(cameraYaw, float.MinValue, float.MaxValue);
        cameraPitch = ClampAngle(cameraPitch, minPitch, maxPitch);

        // Cinemachine will follow this target
        cmCameraTarget.transform.rotation = Quaternion.Euler(cameraPitch + CameraAngleOverride,
            cameraYaw, 0.0f);
    }

    //CHARACTER VISIBILITY
    public void SetCharacterVisibility(bool visibility) {
        characterMesh.SetActive(false);
    }
    public void ToggleCharacterVisibility() {
        characterMesh.SetActive(!characterMesh.activeSelf);
    }

    //UTILS
    private static float ClampAngle(float lfAngle, float lfMin, float lfMax) {
        if (lfAngle < -360f) lfAngle += 360f;
        if (lfAngle > 360f) lfAngle -= 360f;
        return Mathf.Clamp(lfAngle, lfMin, lfMax);
    }
    public Vector3 MouseToWorld(Vector2 mousePosition) {
        //Make a ray from the camera in the cursor direction
        Ray r = Camera.main.ScreenPointToRay(mousePosition);
        //Make a horizontal plane at the height of the cameraTarget
        Plane p = new Plane(Vector3.up, transform.position);
        //Get the point at which the ray collides with the plane
        float distance;
        p.Raycast(r, out distance);
        Vector3 result = r.origin + r.direction.normalized * distance;
        Debug.DrawLine(r.origin, result);
        return result;
    }
    public Vector3 GetMouseWorldPosition() {
        return MouseToWorld(input.mousePosition);
    }

    //TODO - NOT NEEDED ANYMORE ---------------
    private void Attack() {
        //Vector3 pos = Camera.main.ScreenToWorldPoint(input.mousePosition);
        Vector3 lookDirection = MouseToWorld(input.mousePosition) - this.transform.position;
        //Ignore the verticality
        lookDirection.y = 0;
        //Make the character look towards the attack direction
        Quaternion lookRotation = Quaternion.LookRotation(lookDirection.normalized, Vector3.up);
        SetCharacterRotation(lookRotation);
        //Set the animator properties
        animator.SetBool("isChargingAttack", true);
        isChargingAttack = true;
        animator.SetBool("isIdling", false);
        animator.SetBool("isRunning", false);
    }
    private void UpdateAttackStates() {
        if (isChargingAttack) {
            print("chargingAttack");
            animator.SetBool("isChargingAttack", false);
            isChargingAttack = false;
            animator.SetBool("isAttacking", true);
            isAttacking = true;
        }
    }

}