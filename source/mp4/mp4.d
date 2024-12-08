// Basic MP4 Parsing stuff
module mp4.mp4;

import std.stdio;
import std.algorithm;
import std.array;
import std.traits;
import std.range;
import std.conv : to;

import remapped.remapped : remapped, totalFieldSize;

struct AtomHeader {
    //todo handle extended sizes
    ulong size;
    char[4] type;

    ulong offset; //where does it (should it) occur
}


//accepts prettyPrint!RealStruct(remapped!RealStruct(data))
void prettyPrint(T, U)(const auto ref U val, int indentLevel = 0){
    string prefix = repeat(" ", indentLevel*2).joiner("").to!string;
    writeln(prefix, T.stringof, "(");
    static foreach(fieldName; FieldNameTuple!T){
        writeln(prefix, "  ", fieldName, ": ", __traits(getMember, val, fieldName));
    }

    writeln(prefix, ")");
}

//works as expeted
void prettyPrint(T)(const auto ref T val, int indentLevel = 0){
    prettyPrint!(T,T)(val, indentLevel);
}

// annotate the atom data, used in dumping, maybe other places in the future
struct NamedAtom {string name;}

// annotate fields as being arrays, also for dumping, right now
struct ArrayOf(T){ alias ElementType = T;}

@NamedAtom("ftyp")
struct FileTypeLayout {
    char[4] brand;
    uint version_;
    //list of compatible brands for the rest of the atom
}

@NamedAtom("mvhd")
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

@NamedAtom("tkhd")
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

@NamedAtom("elst")
struct EditListHeaderLayout {
    ubyte version_;
    ubyte[3] flags;

    @ArrayOf!EditListEntry
    uint numEntries;

}


struct EditListEntry {
    int trackDuration;
    int mediaTime;
    int mediaRate;
}


@NamedAtom("mdhd")
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

@NamedAtom("hdlr")
struct HandlerLayout {
    ubyte version_;
    ubyte[3] flags;
    char[4] componentType;
    char[4] componentSubtype;
    uint componentManufacturer;
    uint componentFlags;
    uint componentFlagMask;
    //cstring after this, I guess?  "counted string but based on hex dump,
    //I don't see a length field
}


@NamedAtom("vmhd")
struct VideoMediaInfoLayout {
    ubyte version_;
    ubyte[3] flags;
    ushort graphicsMode;
    ubyte[6] opcolor; //really ushort[3], but ignoring that
}

@NamedAtom("smhd")
struct SoundMediaInfoLayout {
    ubyte version_;
    ubyte[3] flags;
    ushort balance;
    ushort reserved;
}


@NamedAtom("dref")
struct DataReferenceLayout {
    ubyte version_;
    ubyte[3] flags;
    // can't do a normal array cast here, since the children are
    // variably sized
    //ideally @ArraryOf...
    uint numEntries;
}

@NamedAtom("stsd")
struct SampleDescriptionLayout {
    ubyte version_;
    ubyte[3] flags;
    // can't do a normal array cast here, since the children are
    // variably sized
    //ideally @ArraryOf...

    uint numEntries;
}

@NamedAtom("stts")
struct TimeToSampleLayout {
    ubyte version_;
    ubyte[3] flags;
    uint numEntries;
}

@NamedAtom("stsc")
struct SampleToChunkLayout {
    ubyte version_;
    ubyte[3] flags;
    uint numEntries;
}

@NamedAtom("stss")
struct SyncSampleLayout {
    ubyte version_;
    ubyte[3] flags;

    @ArrayOf!TimeToSampleEntry
    uint numEntries;
}

@NamedAtom("stsz")
struct SampleSizeLayout {
    ubyte version_;
    ubyte[3] flags;
    uint sampleSize;

    @ArrayOf!SampleSizeEntry
    uint numEntries;
}

struct SampleSizeEntry{
    uint sampleSize;
}

@NamedAtom("stco")
struct ChunkOffsetLayout {
    ubyte version_;
    ubyte[3] flags;
    @ArrayOf!ChunkOffsetEntry
    uint numEntries;
}

struct ChunkOffsetEntry{
    uint chunkOffset; //I think uint?, based on a sample file
}

@NamedAtom("sgpd")
struct SampleGroupDescriptionLayout {
    ubyte version_;
    ubyte[3] flags;
    char[4] groupingType;
    uint defaultLength;

    @ArrayOf!RollDistanceEntry
    uint entryCount;
    //data is array of shorts
}

struct RollDistanceEntry{
    short rollDistance; //I think will always be -1 for stuff I care about
}

@NamedAtom("sbgp")
struct SampleToGroupLayout {
    ubyte version_;
    ubyte[3] flags;
    char[4] groupingType;
    //uint defaultLength; apparently wrong in the doc?  Doesn't make sense based on the format

    @ArrayOf!SampleGroupTableEntry
    uint entryCount;
    //data is array of count/index pairs
    //https://developer.apple.com/documentation/quicktime-file-format/sample-to-group_atom
}

struct SampleGroupTableEntry {
    uint sampleCount;
    uint groupDescriptionIndex;
}

@NamedAtom("sdtp")
struct SampleDependencyFlagsLayout {
    ubyte version_;
    ubyte[3] flags;
    //number or entries is stsz's numEntries
    //variable length of 1 byte entries
}

@NamedAtom("ctts")
struct CompositionOffsetLayout {
    ubyte version_;
    ubyte[3] flags;
    uint entryCount;
}

struct TimeToSampleEntry {
    uint count;
    uint duration;
}

struct SampleToChunkEntry {
    uint firstChunk;
    uint samplesPerChunk;
    uint sampleDescriptionID;
}


//atom header for 'alis', 'rsrc', or 'url ', + version, flags, ubyt[]
struct DataReferenceEntry {
    uint size;
    ubyte[4] type; //really char[4]

}

struct ContainerAtom {
    AtomHeader header;
    alias this = header;
    AtomHeader[] children;
}


//todo enum template?
bool isContainerAtom(T)(auto ref T name){
    static const containers = ["dinf", "edts", "mdia","minf","moov", "stbl", "trak"];
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
            dumpAtom(header, level);
            if(header.type.isContainerAtom){
                auto container = toContainer(header);
                container.children.each!((AtomHeader x){
                        dumpHelper(x, level +1);
                    });
            }
        }

        headers.each!(x => dumpHelper(x, 0));
        dumpAtom(AtomHeader.init);
    }

    void dumpAtom(AtomHeader header, int indentLevel = 0){
        alias thisModule = __traits(parent, NamedAtom);
        alias namedAtoms = getSymbolsByUDA!(thisModule, NamedAtom);
        pragma(msg, "num named atoms: ");
        pragma(msg, namedAtoms.length);
        bool foundAny = false;
        static foreach(i, atomType; namedAtoms){
            pragma(msg, namedAtoms[i]);
            if(!foundAny && header.type == getUDAs!(atomType, NamedAtom)[0].name){{
                    auto atomBody = atomData!atomType(header);
                    prettyPrint!atomType(atomBody, indentLevel);

                static foreach(j, symbol; getSymbolsByUDA!(atomType, ArrayOf)){{
                        pragma(msg, "   " ~ symbol.stringof);
                        alias elementType = getUDAs!(symbol, ArrayOf)[0].ElementType;
                        pragma(msg, "Array of " ~ getUDAs!(symbol, ArrayOf)[0].ElementType.stringof);
                        //alias symbolGetter = __traits(getMember, atomBody, symbol.stringof);
                        auto start = header.offset + 8 + totalFieldSize!atomType;
                        auto mappedArray =
                            remapped!(elementType[])(data[ start ..
                                                           //using this hack because typeof(atomBody)
                                                           //isn't the same as atomType
                                                           start + mixin("atomBody." ~ symbol.stringof)*
                                                          totalFieldSize!elementType]);

                        mappedArray[0 .. min(5, mappedArray.length)].each!(x =>
                                                                            prettyPrint!elementType(x, indentLevel +1));

                    }}
               foundAny = true;
                }}
        }
        //if(!foundAny){ } //anything?
    }

}
