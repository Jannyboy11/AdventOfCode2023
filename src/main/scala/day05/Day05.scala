package day05

import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day05.example")
val sections = source.mkString.split("\n\n")
var seeds: Seq[Long] = sections(0) match
    case s"seeds: ${seedsStr}" => seedsStr.split(" ").map(_.toLong)
val maps: Maps = {
    val mapBuilder = Seq.newBuilder[Seq[MapEntry]]
    for (i <- 1 until sections.length) {
        mapBuilder.addOne(sections(i).split("\n").tail.map(line => {
            val Array(dst, src, len) = line.split(" ");
            MapEntry(dst.toLong, src.toLong, len.toLong) }).toSeq)
    }
    mapBuilder.result()
}

type Source = Long
type Destination = Long
case class MapEntry(destinationStart: Destination, sourceStart: Source, rangeLength: Long):
    def sourceEnd: Long = sourceStart + rangeLength
    def destinationEnd: Long = destinationStart + rangeLength
    def containsSource(value: Long): Boolean = sourceStart <= value && value < sourceEnd
    def distance: Long = destinationStart - sourceStart
type Maps = Seq[Seq[MapEntry]]
object SeedRange:
    def fromUntil(start: Long, end: Long): SeedRange = SeedRange(start, end - start)
case class SeedRange(start: Long, length: Long):
    def end: Long = start + length
    def contains(value: Long): Boolean = start <= value && value < end
    def translate(by: Long): SeedRange = SeedRange(start + by, length)

def mapping(map: Seq[MapEntry])(src: Source): Destination = map
    .find { case MapEntry(dstStart, srcStart, len) => srcStart <= src && src < srcStart + len }
    .map { case MapEntry(dstStart, srcStart, len) => dstStart + (src - srcStart) }
    .getOrElse(src)

def findLocation(maps: Maps, seed: Source): Destination =
    maps.foldLeft[Source => Destination](identity)((acc, map) => acc andThen mapping(map)).apply(seed)

def seedRanges(seeds: Seq[Long]): Set[SeedRange] =
    seeds.grouped(2).map { case Seq(start, length) => SeedRange(start, length) }.toSet

def overlapping(mapEntry: MapEntry, seedRange: SeedRange): Boolean =
    mapEntry.containsSource(seedRange.start) || mapEntry.containsSource(seedRange.end) ||
        seedRange.contains(mapEntry.sourceStart) || seedRange.contains(mapEntry.sourceEnd)

def mapping(map: Seq[MapEntry])(seedRange: SeedRange/*SRC*/): Set[SeedRange/*DST*/] = {
    val applicableMapEntries = map.filter(overlapping(_, seedRange))
    if applicableMapEntries.isEmpty then
        Set(seedRange)
    else
        applicableMapEntries.flatMap(mapEntry => mapRange(mapEntry, seedRange)).toSet
}

def mapRange(mapEntry: MapEntry, seedRange: SeedRange): Set[SeedRange] = {
    if mapEntry.sourceStart <= seedRange.start && seedRange.end <= mapEntry.sourceEnd then
        //seed range is completely subsumed by the map entry
        Set(seedRange.translate(mapEntry.distance))
    else if mapEntry.containsSource(seedRange.start) then
        //seed range extends to the 'right' side, split in two
        //do map [seedRange.start...mapEntry.sourceEnd) to destination
        //do not map [mapEntry.sourceEnd...seedRange.end).
        Set(
            /*mapped part*/ SeedRange.fromUntil(seedRange.start + mapEntry.distance, mapEntry.destinationEnd),
            /*unmapped part*/ SeedRange.fromUntil(mapEntry.sourceEnd, seedRange.end),
        )
    else if mapEntry.containsSource(seedRange.end - 1) then
        //seed range extends to the 'left' side, split in two
        //do not map [seedRange.start...mapEntry.sourceStart)
        //do map [mapEntry.sourceStart...seedRange.end)
        Set(
            /*unmapped part*/ SeedRange.fromUntil(seedRange.start, mapEntry.sourceStart),
            /*mapped part*/ SeedRange.fromUntil(mapEntry.destinationStart, seedRange.end + mapEntry.distance)
        )
    else if seedRange.start < mapEntry.sourceStart && mapEntry.sourceEnd < seedRange.end then
        //map entry is completely subsumed by the seed range, split in three: left, middle (mapped), right
        Set(
            /*left part (unmapped)*/ SeedRange.fromUntil(seedRange.start, mapEntry.sourceStart),
            /*middle part (mapped)*/ SeedRange.fromUntil(mapEntry.destinationStart, mapEntry.destinationEnd),
            /*right part (unmapped)*/ SeedRange.fromUntil(mapEntry.sourceEnd, seedRange.end)
        )
    else
        //no overlap at all (should not occur?)
        Set(seedRange)
}

def findLocation(maps: Maps, seeds: Set[SeedRange]): Set[SeedRange] =
    maps.foldLeft(seeds){ case (ranges, mapEntry) => ranges.flatMap(mapping(mapEntry)) }

@main def main(): Unit = {

    val result1 = seeds.map(findLocation(maps, _)).min
    println(result1)

    val result2 = findLocation(maps, seedRanges(seeds)).map(_.start).min
    println(result2)

}