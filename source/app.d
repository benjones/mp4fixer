import std.stdio;
import mp4.mp4;
import std.algorithm;
import std.conv : to;

int main(string[] args)
{
    if(args.length < 2){
        writeln("usage: mp4Fixer <mp4 filename>");
        return 1;
    }

    auto movie = MP4(args[1]);

    movie.headers.each!writeln;
    writeln("\nmoov atom contents: ");
    movie.moovHeaders.each!writeln;

    auto moovHeaderData = movie.moovHeaderData;
    writeln();
    prettyPrint(moovHeaderData);

    writeln("\ntraks");
    movie.traks.each!writeln;

    return 0;
}
