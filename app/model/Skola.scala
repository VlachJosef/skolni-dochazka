package model

case class SlozeniTridy (trida: Trida, zaci: Set[Zak])
case class Skola(slozeniTrid: List[SlozeniTridy])