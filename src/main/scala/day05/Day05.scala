package day05

import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day05.example")
val sections = source.mkString.split("\n\n")
val seeds: Seq[Long] = sections(0) match
    case s"seeds: ${seedsStr}" => seedsStr.split(" ").map(_.toLong)
//val seeds = Seq(79L, 14L)
//val seeds = Seq(2906961955L, 52237479L)
//val seeds = Seq(1600322402L, 372221628L)
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
    mapEntry.containsSource(seedRange.start) || mapEntry.containsSource(seedRange.end-1) ||
        seedRange.contains(mapEntry.sourceStart) || seedRange.contains(mapEntry.sourceEnd-1)

def mapping(map: Seq[MapEntry])(seedRange: SeedRange/*SRC*/): Set[SeedRange/*DST*/] = {
//    val applicableMapEntries = map.filter(overlapping(_, seedRange))
//    if applicableMapEntries.isEmpty then
//        Set(seedRange)
//    else
//        applicableMapEntries.flatMap(mapEntry => translateRange(mapEntry, seedRange)).toSet
    //TODO why? do I need to filter for applicable map entries? shouldn't I let the translateRange function just handle that?
    //TODO if I don't do it, then the program bugs out. which seems to suggest a bug in the translateRange function.
    map.flatMap(mapEntry => translateRange(mapEntry, seedRange)).toSet
}

def translateRange(mapEntry: MapEntry, seedRange: SeedRange): Set[SeedRange] = {
    println("translating range:")
    println("range = " + seedRange + " (end = " + seedRange.end + ")")
    println("translate using mapEntry = " + mapEntry + " (srcEnd = " + mapEntry.sourceEnd + ", distance = " + mapEntry.distance + ")")
    val res: Set[SeedRange] = if mapEntry.containsSource(seedRange.start) && mapEntry.containsSource(seedRange.end-1) then
        //seed range is completely subsumed by the map entry
        println("seed range is completely subsumed by the map entry")
        Set(seedRange.translate(mapEntry.distance))
    else if mapEntry.containsSource(seedRange.start) then
        //seed range extends to the 'right' side, split in two
        println("seed range extends to the 'right' side, split in two")
        //do map [seedRange.start...mapEntry.sourceEnd) to destination
        //do not map [mapEntry.sourceEnd...seedRange.end).
        Set(
            /*mapped part*/ SeedRange.fromUntil(seedRange.start + mapEntry.distance, mapEntry.destinationEnd),
            /*unmapped part*/ SeedRange.fromUntil(mapEntry.sourceEnd, seedRange.end),
        )
    else if mapEntry.containsSource(seedRange.end-1) then
        //seed range extends to the 'left' side, split in two
        println("seed range extends to the 'left' side, split in two")
        //do not map [seedRange.start...mapEntry.sourceStart)
        //do map [mapEntry.sourceStart...seedRange.end)
        Set(
            /*unmapped part*/ SeedRange.fromUntil(seedRange.start, mapEntry.sourceStart),
            /*mapped part*/ SeedRange.fromUntil(mapEntry.destinationStart, seedRange.end + mapEntry.distance)
        )
    else if seedRange.contains(mapEntry.sourceStart) && seedRange.contains(mapEntry.sourceEnd-1) then
        //map entry is completely subsumed by the seed range, split in three: left, middle (mapped), right
        println("map entry is completely subsumed by the seed range, split in three: left, middle (mapped), right")
        Set(
            /*left part (unmapped)*/ SeedRange.fromUntil(seedRange.start, mapEntry.sourceStart),
            /*middle part (mapped)*/ SeedRange.fromUntil(mapEntry.destinationStart, mapEntry.destinationEnd),
            /*right part (unmapped)*/ SeedRange.fromUntil(mapEntry.sourceEnd, seedRange.end)
        )
        //TODO correct?
    else
        //no overlap at all (should not occur?)
        println("no overlap at all")
        //assert(false)
        Set(seedRange)

    println("result = " + res.map { r => r.toString + " (end = " + r.end + ")" })
    println()
    res
}

def findLocation(maps: Maps, seeds: Set[SeedRange]): Set[SeedRange] =
    maps.foldLeft(seeds){ case (ranges, mapEntry) => ranges.flatMap { case (range) => mapping(mapEntry)(range) } }

@main def main(): Unit = {

    val result1 = seeds.map(findLocation(maps, _)).min
    println(result1)

    val result2 = findLocation(maps, seedRanges(seeds)).map(range => range.start).min
    println(result2)    //126821306 is too high

}