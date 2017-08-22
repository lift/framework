package net.liftweb.http

/**
 * A trait that provides *actual* serialization of a type so that
 * the type can be stored into a container's session and be migrated across
 * servers
 */
trait ContainerSerializer[T] {
  def serialize(in: T): Array[Byte]

  def deserialize(in: Array[Byte]): T
}

object ContainerSerializer {

  import java.util.Date
  import org.joda.time.DateTime

  private def buildSerializer[T]: ContainerSerializer[T] =
    new ContainerSerializer[T] {

      import java.io._

      def serialize(in: T): Array[Byte] = {
        val bos = new ByteArrayOutputStream()
        val oos = new ObjectOutputStream(bos)
        oos.writeObject(in)
        oos.flush()
        bos.toByteArray()
      }

      def deserialize(in: Array[Byte]): T = {
        val bis = new ByteArrayInputStream(in)
        val ois = new ObjectInputStream(bis)
        ois.readObject.asInstanceOf[T]
      }
    }

  implicit val objectSerializer: ContainerSerializer[Object] = buildSerializer
  implicit val intSerializer: ContainerSerializer[Int] = buildSerializer
  implicit val longSerializer: ContainerSerializer[Long] = buildSerializer
  implicit val charSerializer: ContainerSerializer[Char] = buildSerializer
  implicit val shortSerializer: ContainerSerializer[Short] = buildSerializer
  implicit val byteSerializer: ContainerSerializer[Byte] = buildSerializer
  implicit val floatSerializer: ContainerSerializer[Float] = buildSerializer
  implicit val doubleSerializer: ContainerSerializer[Double] = buildSerializer
  implicit val booleanSerializer: ContainerSerializer[Boolean] = buildSerializer
  implicit val dateSerializer: ContainerSerializer[Date] = buildSerializer
  implicit val stringSerializer: ContainerSerializer[String] = buildSerializer
  implicit val jodaDateSerializer: ContainerSerializer[DateTime] = buildSerializer

  implicit def arraySerializer[T](implicit tc: ContainerSerializer[T]): ContainerSerializer[Array[T]] = buildSerializer

  implicit def listSerializer[T](implicit tc: ContainerSerializer[T]): ContainerSerializer[List[T]] = buildSerializer

  implicit def anyRefSerializer[T <: Serializable]: ContainerSerializer[T] = buildSerializer
}