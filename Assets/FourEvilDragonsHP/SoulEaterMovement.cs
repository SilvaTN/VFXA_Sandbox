using UnityEngine;

public class SoulEaterMovement : MonoBehaviour
{
    [SerializeField] private Animator dragonAnimator;

    void Update()
    {

        if (Input.GetKeyDown(KeyCode.Space))
        {
            Debug.Log("Pressed Space");
            dragonAnimator.SetTrigger(AnimatorParameters.IsAttacking);
        }
        else if (Input.GetKey(KeyCode.W))
        {
            Debug.Log("Pressed W");
            dragonAnimator.SetBool(AnimatorParameters.IsIdle, false);
            dragonAnimator.SetBool(AnimatorParameters.IsMoving, true);
        }
        else if (Input.GetKeyDown(KeyCode.UpArrow))
        {
            dragonAnimator.SetBool(AnimatorParameters.IsFlying, true);
        }
        else if (Input.GetKeyDown(KeyCode.DownArrow))
        {
            dragonAnimator.SetBool(AnimatorParameters.IsFlying, false);
        }
        else
        {
            dragonAnimator.SetBool(AnimatorParameters.IsIdle, true);
            dragonAnimator.SetBool(AnimatorParameters.IsMoving, false);
        }
    }
}
