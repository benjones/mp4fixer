// Basic MP4 Parsing stuff
module mp4.mp4;

import std.stdio;
import std.algorithm;
import std.array;
import std.traits;

import remapped.remapped : remapped;

struct AtomHeader {
    //todo handle extended sizes
    ulong size;
    char[4] type;

    ulong offset; //where does it (should it) occur
}


void prettyPrint(T)(const ref T val){
    writeln(T.stringof, "(");

    static foreach(fieldName; FieldNameTuple!T){
        writeln(fieldName, ": ", __traits(getMember, val, fieldName));
    }

    writeln(")");
}


struct MVHDLayout {
    ubyte version_;
    ubyte[3] flags;
    uint creationTime;
    uint modificationTime;
    uint timeScale;
    uint duration;
    uint preferredRate;
    ushort preferredVolume;
    ubyte[10] reserved;
    ubyte[36] matrixStructure;
    uint previewTime;
    uint previewDuration;
    uint posterTime;
    uint selectionTime;
    uint selectionDuration;
    uint currentTime;
    uint nextTrackId;
}

struct Trak {
    AtomHeader[] atoms;
}

struct MP4 {
    import std.mmfile;

    AtomHeader[] headers;
    AtomHeader[] moovHeaders;

    Trak[] traks;

    MmFile mmFile;
    ubyte[] data;

    this(string filename){



        mmFile = new MmFile(filename, MmFile.Mode.read, 0, null);

        data = cast(ubyte[])(mmFile[]);
        headers = parseAtoms(0, data.length);

        auto moovs = headers.filter!(x => x.type == "moov"[0 ..4]).array;
        if(moovs.empty){
            writeln("no moov atom");
        } else {
            assert(moovs.length == 1);
            auto moovHeader = moovs.front;
            with(moovHeader){
                //+8 so we don't visit the moov atom's header
                moovHeaders = parseAtoms(offset + 8,offset + size);

                traks = moovHeaders.filter!(x => x.type == "trak"[0..4])
                    .map!(x => Trak(parseAtoms(x.offset + 8, x.offset + x.size)))
                    .array;
            }
        }

    }

    //todo, use start/stop rather than slice for absolute offsets?
    AtomHeader[] parseAtoms(ulong start, ulong stop){
        import std.bitmanip;

        AtomHeader[] headers;
        ulong nextHeader = start;
        while((nextHeader + 8) < stop){
            auto header = AtomHeader();
            header.size = data[nextHeader .. nextHeader + 4].peek!int;
            header.type = cast(char[4])(data[nextHeader + 4 .. nextHeader + 8][0 ..4]);
            if(header.size == 0){
                //special "last atom" marker
                nextHeader = data.length;
            } else if(header.size == 1){
                header.size = data[nextHeader + 8 .. nextHeader + 16].peek!ulong;
            }
            header.offset = nextHeader;

            nextHeader += header.size; //don't include size of this header
            headers ~= header;
        }
        if(nextHeader != stop){
            writeln("header expected ", nextHeader - stop, " bytes after end of region");
        }
        return headers;
    }

    MVHDLayout moovHeaderData(){
        auto ret = MVHDLayout();
        auto filtered = moovHeaders.filter!(x => x.type == "mvhd");
        if(filtered.empty) return ret;

        auto mvhdHeader = filtered.front;
        auto wrapper = remapped!MVHDLayout(data[mvhdHeader.offset + 8 .. $]);
        ret = cast(MVHDLayout)wrapper;
        return ret;
    }
}
