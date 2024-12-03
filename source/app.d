import std.stdio;
import mp4.mp4;
import std.algorithm;
import std.range;
import std.conv : to;

int main(string[] args)
{
    if(args.length < 2){
        writeln("usage: mp4Fixer <mp4 filename>");
        return 1;
    }

    auto movie = MP4(args[1]);

    movie.headers.each!writeln;
    /*writeln("\nmoov atom contents: ");
    movie.moov.children.each!writeln;

    auto moovHeaderData = movie.moovHeaderData;
    writeln();
    prettyPrint(moovHeaderData);

    writeln("\ntraks");
    movie.traks.each!writeln;
    */

    writeln("\n\ntesting at methods");

    auto mHeader = movie.toContainer(movie.at(["moov"]).front);
    writeln("moov container: " ~ to!string(mHeader));
    mHeader.children.named("trak").each!( (trak){
            writeln(trak);
            auto minf = movie.toContainer(movie.at(trak, ["mdia","minf"]).front);
            writeln(minf);
        });


    return 0;
}
