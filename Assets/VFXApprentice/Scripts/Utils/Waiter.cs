using System.Collections;
using System;
using System.Collections.Generic;
using UnityEngine;

public class Waiter : MonoBehaviour
{
    public static IEnumerator WaitAndDo(Action function, float time) {
        yield return new WaitForSeconds(time);
        function();
    }
    public static IEnumerator WaitAndDoRealtime(Action function, float time) {
        yield return new WaitForSecondsRealtime(time);
        function();
    }
}
