package day05

import scala.io.Source

val source = Source.fromResource("day05.in")
val sections = source.mkString.split("\n\n")
val seeds: Seq[Long] = sections(0) match
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
    override def toString: String = s"MapEntry(srcStart=$sourceStart,srcEnd=$sourceEnd,dstStart=$destinationStart,distance=$distance)"
type Maps = Seq[Seq[MapEntry]]
object SeedRange:
    def fromUntil(start: Long, end: Long): SeedRange = SeedRange(start, end - start)
case class SeedRange(start: Long, length: Long):
    def end: Long = start + length
    def contains(value: Long): Boolean = start <= value && value < end
    def translate(by: Long): SeedRange = SeedRange(start + by, length)
    override def toString: String = s"SeedRange(start=$start,end=$end)"

def mapping(map: Seq[MapEntry])(src: Source): Destination = map
    .find { case MapEntry(dstStart, srcStart, len) => srcStart <= src && src < srcStart + len }
    .map { case MapEntry(dstStart, srcStart, len) => dstStart + (src - srcStart) }
    .getOrElse(src)

def findLocation(maps: Maps, seed: Source): Destination =
    maps.foldLeft[Source => Destination](identity)((acc, map) => acc andThen mapping(map)).apply(seed)

def seedRanges(seeds: Seq[Long]): Set[SeedRange] =
    seeds.grouped(2).map { case Seq(start, length) => SeedRange(start, length) }.toSet

enum SplitResult:
    case Matched(seedRange: SeedRange, distance: Long)
    case Unmatched(seedRange: SeedRange)

def translateRange(usingMapEntries: Seq[MapEntry])(seedRange: SeedRange): Set[SeedRange] = {
    val res = scala.collection.mutable.Set.empty[SeedRange]

    val unmatchedRanges = scala.collection.mutable.Queue.empty[SeedRange]
    unmatchedRanges.addOne(seedRange)

    for mapEntry <- usingMapEntries do
        val unmatchedRangesNext = scala.collection.mutable.Set.empty[SeedRange]
        while unmatchedRanges.nonEmpty do
            val unmatchedRange: SeedRange = unmatchedRanges.removeHead(false)
            for splitResult <- calculatedSplits(mapEntry, unmatchedRange) do
                splitResult match
                    case SplitResult.Matched(range, distance) =>
                        res.addOne(range.translate(distance))
                    case SplitResult.Unmatched(unmatched) =>
                        if unmatchedRange != unmatched then
                            unmatchedRanges.addOne(unmatched)       //add to current queue
                        else
                            unmatchedRangesNext.addOne(unmatched)   //add to queue for next mapEntry
        unmatchedRanges.addAll(unmatchedRangesNext)

    res.addAll(unmatchedRanges)
    res.toSet
}

def calculatedSplits(mapEntry: MapEntry, seedRange: SeedRange): Seq[SplitResult] = {
    if mapEntry.containsSource(seedRange.start) && mapEntry.containsSource(seedRange.end-1) then
        //seed range is completely subsumed by the map entry.
        Seq(SplitResult.Matched(seedRange, mapEntry.distance))
    else if mapEntry.containsSource(seedRange.start - 1) then
        //seed range extends to the 'right' side, split in two.
        //do map [seedRange.start...mapEntry.sourceEnd) to destination.
        //do not map [mapEntry.sourceEnd...seedRange.end).
        Seq(
            SplitResult.Matched(SeedRange.fromUntil(seedRange.start, mapEntry.sourceEnd), mapEntry.distance),
            SplitResult.Unmatched(SeedRange.fromUntil(mapEntry.sourceEnd, seedRange.end))
        )
    else if mapEntry.containsSource(seedRange.end - 1) then
        //seed range extends to the 'left' side, split in two.
        //do not map [seedRange.start...mapEntry.sourceStart).
        //do map [mapEntry.sourceStart...seedRange.end).
        Seq(
            SplitResult.Unmatched(SeedRange.fromUntil(seedRange.start, mapEntry.sourceStart)),
            SplitResult.Matched(SeedRange.fromUntil(mapEntry.sourceStart, seedRange.end), mapEntry.distance)
        )
    else if seedRange.contains(mapEntry.sourceStart) && seedRange.contains(mapEntry.sourceEnd - 1) then
        //map entry is completely subsumed by the seed range, split in three: left, middle (mapped), right.
        Seq(
            SplitResult.Unmatched(SeedRange.fromUntil(seedRange.start, mapEntry.sourceStart)),
            SplitResult.Matched(SeedRange.fromUntil(mapEntry.sourceStart, mapEntry.sourceEnd), mapEntry.distance),
            SplitResult.Unmatched(SeedRange.fromUntil(mapEntry.sourceEnd, seedRange.end))
        )
    else
        //no overlap at all
        Seq(SplitResult.Unmatched(seedRange))
}

def findLocation(maps: Maps, seeds: Set[SeedRange]): Set[SeedRange] =
    maps.foldLeft(seeds){ case (ranges, mapEntries) => ranges.flatMap(translateRange(mapEntries)) }

@main def main(): Unit = {

    val result1 = seeds.map(findLocation(maps, _)).min
    println(result1)

    val result2 = findLocation(maps, seedRanges(seeds)).map(_.start).min
    println(result2)

}