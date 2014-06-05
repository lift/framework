package net.liftweb.actor;

import java.lang.annotation.*;

/**
 * The internal Java class where the annotations for
 * JavaActor are stored
 */
public class JavaActorBase {
    /**
     * A method annotated with Receive will
     * receive a message of the type of its parameter.
     */
    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    public @interface Receive {
    }
}
