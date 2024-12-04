// Basic MP4 Parsing stuff
module mp4.mp4;

import std.stdio;
import std.algorithm;
import std.array;
import std.traits;
import std.range;

import remapped.remapped : remapped;

struct AtomHeader {
    //todo handle extended sizes
    ulong size;
    char[4] type;

    ulong offset; //where does it (should it) occur
}


//accepts prettyPrint!RealStruct(remapped!RealStruct(data))
void prettyPrint(T, U)(const auto ref U val){
    writeln(T.stringof, "(");

    static foreach(fieldName; FieldNameTuple!T){
        writeln("  " ~ fieldName, ": ", __traits(getMember, val, fieldName));
    }

    writeln(")");
}

//works as expeted
void prettyPrint(T)(const auto ref T val){
    prettyPrint!(T,T)(val);
}

// annotate the atom data, for some future
// meta programming reason
struct NamedAtom(string name) {};

@NamedAtom!"mvhd"
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

@NamedAtom!"tkhd"
struct TrackHeaderLayout {
    ubyte version_;
    ubyte[3] flags;
    uint creationTime;
    uint modificationTime;
    uint trackID;
    ubyte[4] reserved;
    uint duration;
    ubyte[8] reserved2;
    ushort layer;
    ushort alternateGroup;
    ushort volume;
    ubyte[2] reserved3;
    ubyte[36] matrixStructure;
    uint trackWidth;
    uint trackHeight;
}

@NamedAtom!"elst"
struct EditListHeaderLayout {
    ubyte version_;
    ubyte[3] flags;
    uint numEntries;
}


struct EditListEntry {
    int trackDuration;
    int mediaTime;
    int mediaRate;
}


@NamedAtom!"mdhd"
struct MediaHeaderLayout {
    ubyte version_;
    ubyte[3] flags;
    uint creationTime;
    uint modificationTime;
    uint timeScale;
    uint duration;
    ushort langauge;
    ushort quality;
}

@NamedAtom!"hdlr"
struct HandlerLayout {
    ubyte version_;
    ubyte[3] flags;
    uint componentType;
    uint componentSubtype;
    uint componentManufacturer;
    uint componentFlags;
    uint componentFlagMask;
    //cstring after this, I guess?  "counted string but based on hex dump,
    //I don't see a length field
}


@NamedAtom!"vmhd"
struct VideoMediaInfoLayout {
    ubyte version_;
    ubyte[3] flags;
    ushort graphicsMode;
    ubyte[6] opcolor; //really ushort[3], but ignoring that
}

@NamedAtom!"smhd"
struct SoundMediaInfoLayout {
    ubyte version_;
    ubyte[3] flags;
    ushort balance;
    ushort reserved;
}

struct ContainerAtom {
    AtomHeader header;
    alias this = header;
    AtomHeader[] children;
}


//todo enum template?
bool isContainerAtom(T)(auto ref T name){
    static const containers = ["mdia","minf","moov", "stbl", "trak"];
    return containers.canFind(name);
}

auto named(HeaderRange)(HeaderRange headers, string name)
     if(isForwardRange!HeaderRange){
         assert(name.length ==4);
         return headers.filter!(x => x.type == name);
}


struct MP4 {
    import std.mmfile;

    AtomHeader[] headers;

    ContainerAtom moov;

    ContainerAtom[] traks;

    MmFile mmFile;
    ubyte[] data;

    this(string filename){



        mmFile = new MmFile(filename, MmFile.Mode.read, 0, null);

        data = cast(ubyte[])(mmFile[]);
        headers = parseAtoms(0, data.length);

        auto moovs = headers.named("moov");
        if(moovs.empty){
            writeln("no moov atom");
        } else {
            moov = toContainer(moovs.front);
            //assume there's just 1

            traks = moov.children.named("trak")
                .map!(x => toContainer(x))
                .array;

            foreach(trak; traks){
                auto trakHeaders = trak.children.named("tkhd");
                if(trakHeaders.empty){
                    writeln("no header for track");
                } else {
                    auto trakHeader = trakHeaders.front;
                    auto trackHeader = atomData!TrackHeaderLayout(trakHeader);
                    prettyPrint(cast(TrackHeaderLayout)trackHeader);
                }

                auto editLists = trak.children.named("edts");
                if(!editLists.empty){
                    auto editList = toContainer(editLists.front);
                    writeln("\nedit list: ");
                    foreach(edit; editList.children){
                        auto mappedHeader = atomData!EditListHeaderLayout(edit);
                        writeln(cast(EditListHeaderLayout)mappedHeader);
                        foreach(i; 0 .. mappedHeader.numEntries){
                            auto entry = cast(EditListEntry)data[edit.offset + 16 .. $].remapped!EditListEntry;
                            prettyPrint(entry);
                        }
                    }

                    writeln();
                }

                trak.children.named("mdia")
                    .map!(x => toContainer(x))
                    .each!( delegate(ContainerAtom media){
                            prettyPrint(media);
                            media.children.named("mdhd")
                                .each!( delegate(AtomHeader mdhd) {
                                        auto mapped = mapAtomContents!MediaHeaderLayout(mdhd);
                                        prettyPrint!MediaHeaderLayout(mapped);
                                        //prettyPrint(cast(MediaHeaderLayout)mapped);
                                    });

                            media.children.named("minf")
                                .each!( (AtomHeader minfHeader) {
                                        auto minf = toContainer(minfHeader);
                                        prettyPrint(minf);
                                    });
                        });



            }
        }

    }

    ContainerAtom toContainer(AtomHeader header){
        ContainerAtom ret;
        ret.header = header;
        ret.children = parseAtoms(header.offset + 8, header.offset + header.size);
        return ret;
    }

    auto mapAtomContents(T)(AtomHeader ah){
        // + 8 to ignore size + type fields
        // should probably handle the case where size is 0...
        assert(ah.size < uint.max);
        return data[ah.offset + 8 .. $].remapped!T;
    }

    auto atomData(T)(AtomHeader header){
        assert(header.size < int.max); //make sure we don't have a 64 bit size
        return data[header.offset + 8 ..$].remapped!T;
    }


    auto at(string[] path) => at(headers, path);
    auto at(ContainerAtom parent, string[] path) => at(parent.children, path);
    auto at(AtomHeader parent, string[] path) => at(toContainer(parent).children, path);

    private auto at(AtomHeader[] root, string[] path){
        auto nextHeaders = root;
        foreach(atom; path[0 .. $ -1]){
            auto children = nextHeaders.named(atom);
            assert(!children.empty);
            auto first = children.front;
            children.popFront;
            assert(children.empty); //should be only 1
            nextHeaders = toContainer(first).children;
        }
        return nextHeaders.named(path[$-1]);
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
        auto filtered = moov.children.filter!(x => x.type == "mvhd");
        if(filtered.empty) return ret;

        auto mvhdHeader = filtered.front;
        auto wrapper = remapped!MVHDLayout(data[mvhdHeader.offset + 8 .. $]);
        ret = cast(MVHDLayout)wrapper;
        return ret;
    }

    void dumpAtomTree(){
        import std.conv : to;
        import std.range;

        void dumpHelper(AtomHeader header, int level){
            string prefix = repeat(" ", level*2).joiner("").to!string;
            writeln(prefix, header.type);
            if(header.type.isContainerAtom){
                auto container = toContainer(header);
                container.children.each!((AtomHeader x){
                        dumpHelper(x, level +1);
                    });
            }
        }

        headers.each!(x => dumpHelper(x, 0));
    }

}
