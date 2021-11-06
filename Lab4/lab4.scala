package Lab4

import scala.annotation.tailrec

object lab4 {

  //Zadanie 1

  def find_one_elements(list: List[String], elements: List[String]): List[String] =
    if list == Nil then Nil
    else if elements == Nil then list
    else if contain_elements(list.head, elements) then list.head :: find_one_elements(list.tail, elements)
    else find_one_elements(list.tail, elements)

  def contain_elements(segment: String, elements: List[String]): Boolean =
    if elements == Nil then false
      else  if contain_element(segment, elements.head)  then true
        else contain_elements(segment, elements.tail)

  def contain_element(segment: String, element: String): Boolean =
    (segment, element) match
      case (_, "") => true
      case ("", _) => false
      case (_, _) =>  if segment.head == element.head  then contain_element(segment.tail, element.tail) else contain_element(segment.tail,element)


  //Zadanie 2

  def joinList[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (h1 :: t1, _, _) => h1 :: joinList(t1, list2, list3)
      case (Nil, h2 :: t2, _) => h2 :: joinList(Nil, t2, list3)
      case (Nil, Nil, _) => list3

  def reverse[A](list: List[A]): List[A] =
    @tailrec
    def helper(list: List[A], acc: List[A]): List[A] =
      list match
        case Nil => acc
        case hd :: tl => helper(tl, hd :: acc)

    helper(list, Nil)

  def joinListTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    @tailrec
    def helper[B](list1: List[B], list2: List[B], list3: List[B], acc: List[B]): List[B] =
      (list1, list2, list3) match
        case (h1 :: t1, _, _) => helper(t1, list2, list3, h1 :: acc)
        case (Nil, h2 :: t2, _) => helper(list1, t2, list3, h2 :: acc)
        case (Nil, Nil, h3 :: t3) => helper(list1, list2, t3, h3 :: acc)
        case (Nil, Nil, Nil) => acc.reverse

    helper(list1, list2, list3, List())

  def main(args: Array[String]): Unit = {

  }
}

