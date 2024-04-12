package ex

import util.Optionals.Optional
import util.Sequences.*
trait Item:
  def code: Int
  def name: String
  def tags: Sequence[String]

object Item:
  def apply(code: Int, name: String, tags: String*): Item =
    var tagSequence: Sequence[String] = Sequence()
    for tag <- tags do
      tagSequence = tagSequence.concat(Sequence(tag))
    ItemImpl(code: Int, name: String, tagSequence: Sequence[String])

  private case class ItemImpl(code: Int, name: String, tags: Sequence[String]) extends Item

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): Sequence[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Optional[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
end Warehouse

object Warehouse:
  import util.Sequences.*

  private var sequence = Sequence[ex.Item]()

  def apply(): Warehouse = WarehouseImpl()

  private case class WarehouseImpl() extends Warehouse:

    def contains(itemCode: Int): Boolean = sequence.map(_.code).contains(itemCode)

    def remove(item: ex.Item): Unit =
      sequence = sequence.filter(_!=item)
    def retrieve(code: Int): util.Optionals.Optional[ex.Item] = sequence.find(item => this.contains(code))
    def searchItems(tag: String): util.Sequences.Sequence[ex.Item] = sequence.filter(_.tags.contains(tag))
    def store(item: ex.Item): Unit =
      sequence = sequence.concat(Sequence(item))

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", tags = "notebook")
  val dellInspiron = Item(34, "Dell Inspiron 13", tags =  "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", tags =  "moped", "mobility")

  println(warehouse.contains(dellXps.code)) // false
  warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  println(warehouse.contains(dellXps.code)) // true
  warehouse.store(dellInspiron) // side effect, add dell Inspiron to the warehouse
  println(warehouse.contains(xiaomiMoped.code)) //false
  warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  println(warehouse.contains(xiaomiMoped.code)) // true
  println(warehouse.searchItems("mobility")) // Sequence(xiaomiMoped)
  println(warehouse.searchItems("notebook")) // Sequence(dellXps, dell Inspiron)
  println("Retrive "+warehouse.retrieve(11)) // None
  println(warehouse.retrieve(dellXps.code)) // Just(dellXps)
  warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  println(warehouse.retrieve(dellXps.code)) // None

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private List of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/