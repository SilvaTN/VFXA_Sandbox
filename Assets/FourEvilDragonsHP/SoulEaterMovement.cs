using System.Collections;
using UnityEngine;

public class SoulEaterMovement : MonoBehaviour
{
    [SerializeField] private Animator dragonAnimator;
    private float currentSpeed;
    [SerializeField] private float runSpeed;
    [SerializeField] private float flySpeed;
    private float currentRotationSpeed;
    [SerializeField] private float runRotationSpeed;
    [SerializeField] private float flyRotationSpeed;
    [SerializeField] private ParticleSystem projectilePS;
    [SerializeField] private float projectilePSDelay;
    [SerializeField] Projectile projectileScript;

    private void Start()
    {
        if (runSpeed == 0f) runSpeed = 7f;
        if (flySpeed == 0f) flySpeed = 14f;
        currentSpeed = runSpeed;

        if (runRotationSpeed == 0f) runRotationSpeed = 5f;
        if (flyRotationSpeed == 0f) flyRotationSpeed = 10f;
        currentRotationSpeed = runRotationSpeed;
    }

    void Update()
    {

        // DEPRECATED: Get the horizontal and vertical axis. I changed the default to just be WASD.
        // The value is in the range -1 to 1
        float translation = Input.GetAxis("Vertical") * currentSpeed;
        float rotation = Input.GetAxis("Horizontal") * currentRotationSpeed;

        // Make it move X meters per second instead of X meters per frame...
        translation *= Time.deltaTime;
        rotation *= Time.deltaTime;

        // move while NOT spitting fire ball
        if (!dragonAnimator.GetCurrentAnimatorStateInfo(0).IsName(AnimatorParameters.FireballGround) 
            && !dragonAnimator.GetCurrentAnimatorStateInfo(0).IsName(AnimatorParameters.FireballFly)) 
        {
            transform.Translate(0f, 0f, translation); // Move translation along the object's z-axis        
            transform.Rotate(0f, rotation, 0f); // Rotate around our y-axis
        }

        if (Input.GetKeyDown(KeyCode.Space))
        {
            Debug.Log("Pressed Space");
            dragonAnimator.SetTrigger(AnimatorParameters.IsAttacking);
            StartCoroutine(PlayAttackPS());
        }
        else if (translation != 0f) //(Input.GetKey(KeyCode.W))
        {
            dragonAnimator.SetBool(AnimatorParameters.IsIdle, false);
            dragonAnimator.SetBool(AnimatorParameters.IsMoving, true);
        }        
        else if (Input.GetKeyDown(KeyCode.UpArrow))
        {
            currentSpeed = flySpeed;
            currentRotationSpeed = flyRotationSpeed;
            dragonAnimator.SetBool(AnimatorParameters.IsFlying, true);
        }
        else if (Input.GetKeyDown(KeyCode.DownArrow))
        {
            currentSpeed = runSpeed;
            currentRotationSpeed = runRotationSpeed;
            dragonAnimator.SetBool(AnimatorParameters.IsFlying, false);
        }
        else if (rotation != 0f)
        {
            dragonAnimator.SetBool(AnimatorParameters.IsIdle, false);
            dragonAnimator.SetBool(AnimatorParameters.IsMoving, false);
            dragonAnimator.SetBool(AnimatorParameters.IsRotating, true);
        }
        else
        {
            dragonAnimator.SetBool(AnimatorParameters.IsIdle, true);
            dragonAnimator.SetBool(AnimatorParameters.IsMoving, false);
            dragonAnimator.SetBool(AnimatorParameters.IsRotating, false);
        }        

    }

    private IEnumerator PlayAttackPS()
    {        
        yield return new WaitForSeconds(projectilePSDelay);
        projectileScript.ReattachToParent();
        projectilePS.Play();
    }
}
