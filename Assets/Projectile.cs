using UnityEngine;

public class Projectile : MonoBehaviour
{
    [SerializeField] private float speed = 25f; // Movement speed in units per second
    private ParticleSystem projectilePS; // The particle system component
    private Transform originalParent;
    private Vector3 originalPosition;
    private Quaternion originalRotation;

    void Start()
    {
        // Get the ParticleSystem component
        projectilePS = GetComponent<ParticleSystem>();

        // Store the original parent and position
        originalParent = transform.parent;
        originalPosition = transform.localPosition;
        originalRotation = transform.localRotation;

        // Reattach to the original parent and reset position
        ReattachToParent();
    }

    void Update()
    {
        // Check if the particle system is playing
        if (projectilePS != null && projectilePS.isPlaying)
        {
            // Detach and move the particle system forward along the X-axis
            transform.parent = null;
            transform.position += Vector3.forward * speed * Time.deltaTime;
        }
    }

    public void ReattachToParent()
    {
        // Reattach to the original parent and reset the position
        transform.parent = originalParent;
        transform.localPosition = originalPosition;
        transform.localRotation = originalRotation;
    }
}
