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

    //in my examples, the body is {size 12, "url ", version + flags == 0, no data}
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

// https://developer.apple.com/documentation/quicktime-file-format/video_sample_description
// Applies to more general video description stuff, but I'll just name it avc1
// since that what I'll get for presenterMode videos
@NamedAtom("avc1")
struct avc1DescriptionLayout{
    ubyte[6] reserved;
    ushort dataReferenceIndex;
    ushort version_;
    ushort revision;
    char[4] vendor;
    uint temporalQuality;
    uint spatialQuality;
    ushort width;
    ushort height;
    uint horizontalResolution; //fixed point, pixels per inch
    uint verticalResolution;
    uint dataSize;
    ushort frameCount; //per sample
    ubyte[32] compressorName; //first byte is length, rest are chars
    ushort pixelDepth;
    short colorTableId;

}

@NamedAtom("avcC")
struct avcC{
    //payload is one of the structs below
    //this might not need to exist?
}

// From ISO 14496 part 15 (the H264 standard)
// Truely nightmarish format
struct AvcDecoderConfigurationRecord {
    ubyte configurationVersion;
    ubyte avcProfileIndication;
    ubyte profileCompatibility;
    ubyte avcLevelIndication;
    ubyte lengthMinus1; //encoded with 6 upper bits set to 1, so only 2 bits used
    ubyte numSequenceParameterSets; //top 3 bits set to 1, lower 5 used

    //variably sized sequence parameter set:
    // format is a ushort of number of bytes, then the variable sized value
    ubyte[][] sequenceParameterSetNALUnits;

    ubyte numPictureElementSets;
    //same as above, each element prefixed with size as a ubyte
    ubyte[][] pictureParameterSetNALUnits;

    //optional part for profileIndications of 100, 110, 122, or 144

    ubyte chromaFormat; //upper 6 bits set to 1's
    ubyte bitDepthLumaMinus8; //top 5 bits set to 1
    ubyte bitDepthChromaMinus8; //same
    ubyte numSequenceParameterSetExtensions;

    //each prefixed with ushort length
    ubyte[][] sequenceParameterSetExtNALUnits;


    this(ubyte[] data){
        import std.bitmanip;
        configurationVersion = data.read!ubyte;
        avcProfileIndication = data.read!ubyte;
        profileCompatibility = data.read!ubyte;
        avcLevelIndication = data.read!ubyte;

        lengthMinus1 = data.read!ubyte;
        assert((lengthMinus1 & 0xFC0) != 0);
        lengthMinus1 &= 3;

        numSequenceParameterSets = data.read!ubyte;
        assert((numSequenceParameterSets & 0xE0) != 0);
        numSequenceParameterSets &= 0x1F;

        foreach(i; 0 .. numSequenceParameterSets){
            ushort size = data.read!ushort;
            sequenceParameterSetNALUnits ~= data[0 .. size];
            data = data[size .. $];
        }

        numPictureElementSets = data.read!ubyte;
        foreach(i; 0 .. numPictureElementSets){
            ushort size = data.read!ushort;
            pictureParameterSetNALUnits ~= data[0 .. size];
            data = data[size .. $];
        }

        if([110, 110, 122, 144].canFind(avcProfileIndication)){

            chromaFormat = data.read!ubyte;
            assert(chromaFormat & 0xFC);
            chromaFormat &= 0x3;

            bitDepthLumaMinus8 = data.read!ubyte;
            assert(bitDepthLumaMinus8 & 0xF8);
            bitDepthLumaMinus8 &= 7;

            bitDepthChromaMinus8 = data.read!ubyte;
            assert(bitDepthChromaMinus8 & 0xF8);
            bitDepthChromaMinus8 &= 7;

            numSequenceParameterSetExtensions = data.read!ubyte;
            foreach(i; 0 ..numSequenceParameterSetExtensions){
                ushort size = data.read!ushort;
                sequenceParameterSetExtNALUnits ~= data[0 .. size];
                data = data[size .. $];
            }
        }

        assert(data.length == 0);

    }
}


@NamedAtom("colr")
struct ColorParameterLayout{
    char[4] parameterType; //should be "nclc" or "nclx"
    ushort primariesIndex; // shoudl be 1
    ushort transferFunctionIndex; // should be 1
    ushort matrixIndex; //should be 1
    ubyte swingType; // ??? ,only for NCLX, probably missing for NCL
}

@NamedAtom("fiel")
struct FieldHandlingLayoud{
    ubyte fieldCount; //1 or 2
    ubyte fieldOrdering; // probably not used
}

@NamedAtom("chrm")
struct ChromticityLayout {
    ushort noIdea; //?? couldn't find doc for it
}


//mp4a audio stuff
// I think this?  https://developer.apple.com/documentation/quicktime-file-format/sound_sample_description_version_0
@NamedAtom("mp4a")
struct mp4aDescriptionLayout {
    ubyte[6] reserved; // must be 0
    ushort dataReferenceIndex;
    ushort version_;
    ushort revisionLevel;
    char[4] vendor;
    ushort numChannels;
    ushort sampleSize;
    ushort compressionID;
    ushort packetSize;
    uint sampleRate; //16.16 fixed point, 0xBB80.0000 == 48khz
}



/*
class DecoderConfigDescriptor extends BaseDescriptor : bit(8) tag=DecoderConfigDescrTag {
   bit(8) objectTypeIndication; //should be value 3 (ES_DescriptorTag)
   //next is the weirdly encoded length keep reading bytes until the MSb is 0
   //concatenate the lower 7 bits of each
   // in this example, we seem to get 0x80808022 (the top 3 are basically padding then the last one counts)
   // 16 bit ES_ID , 0 in my case
   // stream dependency, url, ocr stream flags, then 5 bits padding

   //2 optional ushorts for based on the previous flags, 0 in our case

   //then this stuff ?? apparently not...

   bit(6) streamType;
   bit(1) upStream;
   const bit(1) reserved=1;
   bit(24) bufferSizeDB;
   bit(32) maxBitrate;
   bit(32) avgBitrate;
   DecoderSpecificInfo decSpecificInfo[0 .. 1];
   profileLevelIndicationIndexDescriptor profileLevelIndicationIndexDescr [0..255];
}

found here https://github.com/mono/taglib-sharp/issues/146#issuecomment-464824047

better reference: https://chromium.googlesource.com/chromium/src/media/+/16ba1c56b860d53d7354c0ec9538650cf1f20e2d/mp4/es_descriptor.cc
*/

@NamedAtom("esds")
struct AppleAudioDecoderConfig {
    ubyte version_;
    ubyte[3] flags;
}


@NamedAtom("stts")
struct TimeToSampleLayout {
    ubyte version_;
    ubyte[3] flags;

    @ArrayOf!TimeToSampleEntry
    uint numEntries;
}

struct TimeToSampleEntry {
    uint count;
    uint duration;
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

    @ArrayOf!CompositionOffsetEntry
    uint entryCount;
}

struct CompositionOffsetEntry{
    uint sampleCount;
    uint compositionOffset;
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

struct ContainerWithHeader(T) {
    AtomHeader header;
    ReturnType!(() => remapped!T(cast(ubyte[])[])) containerHeader;
    AtomHeader[] children;

}


//todo enum template?
bool isContainerAtom(T)(auto ref T name){
    static const containers = ["dinf", "edts", "mdia","minf","moov", "stbl", "trak"];
    return containers.canFind(name);
}

static const containersWithHeader = ["avc1", "mp4a", "stsd"];


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
                    auto trackHeader = mapAtomContents!TrackHeaderLayout(trakHeader);
                    prettyPrint(cast(TrackHeaderLayout)trackHeader);
                }

                auto editLists = trak.children.named("edts");
                if(!editLists.empty){
                    auto editList = toContainer(editLists.front);
                    writeln("\nedit list: ");
                    foreach(edit; editList.children){
                        auto mappedHeader = mapAtomContents!EditListHeaderLayout(edit);
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

    auto toContainerWithHeader(HeaderType)(AtomHeader header){
        auto ret = ContainerWithHeader!HeaderType();
        ret.header = header;
        ret.containerHeader = mapAtomContents!HeaderType(header);

        ret.children = parseAtoms(header.offset + 8 + totalFieldSize!HeaderType,
                              header.offset + header.size);
        return ret;
    }

    auto mapAtomContents(T)(AtomHeader ah){
        // + 8 to ignore size + type fields
        // should probably handle the case where size is 0...
        assert(ah.size < uint.max);
        return data[ah.offset + 8 .. ah.offset + ah.size].remapped!T;
    }


    ubyte[] payload(AtomHeader ah){
        return data[ah.offset + 8 .. ah.offset + ah.size];
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

    private alias thisModule = __traits(parent, NamedAtom);
    private alias namedAtoms = getSymbolsByUDA!(thisModule, NamedAtom);

    private template AtomFromName(string name){
        static foreach(atomType; namedAtoms){
            static if(getUDAs!(atomType, NamedAtom)[0].name == name){
                alias AtomFromName = atomType;
            }
        }
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
            } else if(containersWithHeader.canFind(header.type)){
                bool foundAny = false;
                static foreach(containerName; containersWithHeader){
                    if(!foundAny && containerName == header.type){
                        alias ContainerType = AtomFromName!containerName;
                        auto containerWithHeader = toContainerWithHeader!ContainerType(header);
                        prettyPrint!ContainerType(containerWithHeader.containerHeader, level);
                        containerWithHeader.children.each!( atom => dumpHelper(atom, level + 1));
                        foundAny = true;
                    }
                }
                assert(foundAny);
            }
        }

        headers.each!(x => dumpHelper(x, 0));
        dumpAtom(AtomHeader.init);
    }

    void dumpAtom(AtomHeader header, int indentLevel = 0){
        //        pragma(msg, "num named atoms: ");
        //        pragma(msg, namedAtoms.length);
        bool foundAny = false;
        static foreach(i, atomType; namedAtoms){
            pragma(msg, namedAtoms[i]);
            if(!foundAny && header.type == getUDAs!(atomType, NamedAtom)[0].name){{
                    auto atomBody = mapAtomContents!atomType(header);
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
