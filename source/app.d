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
            minf.children.named("vmhd").map!(x => movie.mapAtomContents!VideoMediaInfoLayout(x))
                .each!( vmhd => prettyPrint!VideoMediaInfoLayout(vmhd));

            minf.children.named("smhd").map!(x => movie.mapAtomContents!SoundMediaInfoLayout(x))
                .each!( vmhd => prettyPrint!SoundMediaInfoLayout(vmhd));

            minf.children.named("dinf").map!( x => movie.toContainer(x)).each!( (dinf){
                    dinf.children.named("dref")
                        .each!( (drefHeader){
                                auto dref = movie.mapAtomContents!DataReferenceLayout(drefHeader);
                                prettyPrint!DataReferenceLayout(dref);
                                auto dataEntries = movie.parseAtoms(drefHeader.offset + 16,
                                                                    drefHeader.offset + drefHeader.size);

                                foreach(entry; dataEntries){
                                    writeln(entry);
                                    writeln(cast(char[])movie.data[entry.offset + 8 .. entry.offset + 8 + entry.size]);
                                }
                            });

                });

            auto hdlr = movie.at(trak, ["mdia","hdlr"]);
            hdlr.map!(x => movie.mapAtomContents!HandlerLayout(x))
                .each!(x => prettyPrint!HandlerLayout(x));


        });




    writeln("\n\ndump\n");
    movie.dumpAtomTree();

    return 0;
}
