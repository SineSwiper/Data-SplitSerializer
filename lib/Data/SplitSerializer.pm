package Data::SplitSerializer;

# VERSION
# ABSTRACT: Modules that "split serialize" data structures

#############################################################################
# Modules

use sanity;
use Moo;
use Types::Standard qw(Bool Str HashRef InstanceOf HasMethods);

use Module::Runtime qw( use_module );
use Hash::Merge;
use Try::Tiny;
use Scalar::Util qw( blessed );

use namespace::clean;
no warnings 'uninitialized';

#############################################################################
# Custom Hash::Merge behaviors

my $default_behavior = 'LEFT_PRECEDENT_STRICT_ARRAY_INDEX';

Hash::Merge::specify_behavior(
   {
      # NOTE: Undef is still considered 'SCALAR'.
      SCALAR => {
         SCALAR => sub { $_[1] },
         ARRAY  => sub {
            return $_[1] unless defined $_[0];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'SCALAR', 'ARRAY', $_[0]);
         },
         HASH   => sub {
            return $_[1] unless defined $_[0];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'SCALAR', 'HASH',  $_[0]);
         },
      },
      ARRAY => {
         SCALAR => sub {
            return $_[0] unless defined $_[1];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'ARRAY', 'SCALAR', $_[1]);
         },
         ARRAY  => sub {
            # Handle arrays by index, not by combining
            my ($l, $r) = @_;
            $l->[$_] = $r->[$_] for (
               grep { defined $r->[$_] }
               (0 .. $#{$_[1]})
            );
            return $l;
         },
         HASH   => sub { die sprintf('mismatched type (%s vs. %s) found during merge', 'ARRAY', 'HASH'); },
      },
      HASH => {
         SCALAR => sub {
            return $_[0] unless defined $_[1];
            die sprintf('mismatched type (%s vs. %s) found during merge: $scalar = %s', 'HASH', 'SCALAR', $_[1]);
         },
         ARRAY  => sub { die sprintf('mismatched type (%s vs. %s) found during merge', 'HASH', 'ARRAY'); },
         HASH   => sub { Hash::Merge::_merge_hashes( $_[0], $_[1] ) },
      },
   },
   $default_behavior,
);

#############################################################################
# Attributes

has _merge_obj => (
   is      => 'rw',
   isa     => InstanceOf['Hash::Merge'],
   default => sub { Hash::Merge->new($default_behavior); },
   handles => { qw(
      merge             merge
      specify_behavior  specify_merge_behavior
      set_behavior      set_merge_behavior
   ) },
);

has path_style => (
   is      => 'ro',
   isa     => Str,
   default => sub { 'DZIL' },
   coerce  => sub {
      'Parse::Path::'.$_[0] unless ($_[0] =~ s/^\=//);  # NOTE: kill two birds with one stone
   },
);

has path_options => (
   is      => 'ro',
   isa     => HashRef,
   default => sub { {
      auto_normalize => 1,
      auto_cleanup   => 1,
   } },
);

has remove_undefs => (
   is      => 'ro',
   isa     => Bool,
   default => sub { 1 },
);

#############################################################################
# Pre/post-BUILD

=for Pod::Coverage BUILD
=cut

sub BUILD {
   my $self = $_[0];

   # Load the path class
   use_module $self->path_style;

   return $self;
}

#############################################################################
# Methods

### FLATTENING ###

sub serialize {
   my ($self, $ref) = @_;

   my $type = ref $ref;
   die 'Reference must be an unblessed HASH or ARRAY!'
      unless (defined $ref && !blessed $ref && $type =~ /HASH|ARRAY/);

   return $self->serialize_refpath('', $ref);
}

sub serialize_refpath {
   my ($self, $path, $ref) = @_;
   $path //= '';

   my $prh = { $path => $ref };  # single row answer

   return $prh if blessed $ref;  # down that path leads madness...
   my $type = ref $ref || return $prh;        # that covers SCALARs...
   return $prh unless $type =~ /HASH|ARRAY/;  # ...and all other endpoints

   # Blessed is the path
   unless (blessed $path) {
      $path = $self->path_style->new(
         %{ $self->path_options },
         stash_obj => $self,
         path => $path,
      );
   }

   die sprintf("Too deep down the rabbit hole, stopped at '%s'", $path)
      if ($path->step_count > 255);

   my $hash = {};
   my @keys = $type eq 'HASH' ? (keys %$ref) : (0 .. $#$ref);
   foreach my $key (@keys) {
      my $val = $type eq 'HASH' ? $ref->{$key} : $ref->[$key];

      # Add on to $path
      my $newpath = $path->clone;
      $newpath->push( $newpath->key2hash($key, $type) );

      # Recurse back to give us a full set of $path => $val pairs
      my $newhash = $self->serialize_refpath($newpath, $val);

      # Merge (shallowly)
      $hash->{$_} = $newhash->{$_} for (grep { defined $newhash->{$_} or !$self->remove_undefs } keys %$newhash);
   }

   return $hash;
}

### EXPANSION ###

sub deserialize {
   my ($self, $hash) = @_;

   my $root;  # not sure if it's a hash or array yet
   foreach my $path (sort keys %$hash) {
      my $branch = $self->deserialize_pathval($path, $hash->{$path}) || return;  # error already set

      # New root?
      unless (defined $root) {
         $root = $branch;
         next;
      }

      # Our merge behavior might die on us (or Hash::Merge itself)
      my $err;
      try   { $root = $self->merge($root, $branch); }
      catch { $err = $_; };

      # Add path to error
      die sprintf("In path '%s', %s", $path, $err) if ($err);
   }

   return $root;
}

sub deserialize_pathval {
   my ($self, $path, $val) = @_;

   my ($root, $leaf, $hash_steps);
   $path = $self->path_style->new(
      %{ $self->path_options },
      path => $path,
   );

   for my $i (0 .. $path->step_count - 1) {
      my $hash_step = $path->_path->[$i];
      my $next_step = ($i == $path->step_count - 1) ? undef : $path->_path->[$i+1];

      # Construct $root if we need to
      $root = $leaf = ( $hash_step->{type} eq 'HASH' ? {} : [] ) unless ($i);

      # Add in the key, construct the next ref, and move the leaf forward
      my $type_str = substr($hash_step->{type}, 0, 1);
      $type_str   .= substr($next_step->{type}, 0, 1) if $next_step;

      my $key = $hash_step->{key};

      # (RIP for/when)
      if    ($type_str eq 'HH') { $leaf = $leaf->{$key} = {};   }
      elsif ($type_str eq 'HA') { $leaf = $leaf->{$key} = [];   }
      elsif ($type_str eq 'AH') { $leaf = $leaf->[$key] = {};   }
      elsif ($type_str eq 'AA') { $leaf = $leaf->[$key] = [];   }
      elsif ($type_str eq 'H')  {         $leaf->{$key} = $val; }
      elsif ($type_str eq 'A')  {         $leaf->[$key] = $val; }
   }

   return $root;
}

42;

__END__

=begin wikidoc

= SYNOPSIS

   use Data::SplitSerializer;

   my $dss = Data::SplitSerializer->new( path_style => 'DZIL' );
   my $serialized = {
      'gophers[0].holes'      => 3,
      'gophers[0].food.type'  => 'grubs',
      'gophers[0].food.count' => 7,

      'gophers[1].holes'      => 1,
      'gophers[1].food.type'  => 'fruit',
      'gophers[1].food.count' => 5,
   };
   my $deserialized = $dss->deserialize($serialized);

   my $more_gophers = [];
   $more_gophers->[2] = {
      holes => 2,
      food  => {
         type  => 'earthworms',
         count => 15,
      },
   };

   $deserialized = $dss->merge( $deserialized, $more_gophers );

= DESCRIPTION

Split serialization is a unique form of serialization that only serializes part of the data structure (as a path on the left side) and
leaves the rest of the data, typically a scalar, untouched (as a value on the right side).  Consider the gopher example above:

   my $deserialized = {
      gophers => [
         {
            holes => 3,
            food  => {
               type  => 'grubs',
               count => 7,
            },
         },
         {
            holes => 1,
            food  => {
               type  => 'fruit',
               count => 5,
            },
         },
         {
            holes => 2,
            food  => {
               type  => 'earthworms',
               count => 15,
            },
         }
      ],
   };

A full serializer, like [Data::Serializer] or [Data::Dumper], would turn the entire object into a string, much like the real code
above.  Or into JSON, XML, BerkleyDB, etc.  But, the end values would be lost in the stream.  If you were given an object like this,
how would you be able to store the data in an easy-to-access form for a caching module like [CHI]?  It requires key/value pairs.  Same
goes for [KiokuDB] or various other storage/ORM modules.

Data::SplitSerializer uses split serialization to turn the data into a path like this:

   my $serialized = {
      'gophers[0].holes'      => 3,
      'gophers[0].food.type'  => 'grubs',
      'gophers[0].food.count' => 7,

      'gophers[1].holes'      => 1,
      'gophers[1].food.type'  => 'fruit',
      'gophers[1].food.count' => 5,

      'gophers[2].holes'      => 2,
      'gophers[2].food.type'  => 'earthworms',
      'gophers[2].food.count' => 15,
   };

Now, you can stash the data into whatever storage engine you want... or use just use it as a simple hash.

= CONSTRUCTOR

   # Defaults shown
   my $stash = Data::Stash->new(
      path_style   => 'DZIL',
      path_options => {
         auto_normalize => 1,
         auto_cleanup   => 1,
      },
   );

Creates a new serializer object.  Accepts the following arguments:

== path_style

   path_style => 'File::Unix'
   path_style => '=MyApp::Parse::Path::Foobar'

Class used to create new [path objects|Parse::Path] for path parsing.  With a {=} prefix, it will use that as the full
class.  Otherwise, the class will be intepreted as {Parse::Path::$class}.

Default is [DZIL|Parse::Path::DZIL].

== path_options

   path_options => {
      auto_normalize => 1,
      auto_cleanup   => 1,
   }

Hash of options to pass to new path objects.  Typically, the default set of options are recommended to ensure a more commutative
path.

== remove_undefs

   remove_undefs => 0

Boolean to indicate whether to remove   See [/Undefined values] for more information.

Default is on.

= METHODS

=== serialize

   my $serialized = $dss->serialize($deserialized);

Serializes/flattens a ref.  Returns a serialized hashref of path/value pairs.

=== serialize_refpath

   my $serialized = $dss->serialize_refpath($path_prefix, $deserialized);

   # serialize is basically this with some extra sanity checks
   my $serialized = $dss->serialize_refpath('', $deserialized);

The real workhorse for {serialize_ref}.  Recursively dives down the different pieces of the deserialized tree and eventually comes
back with the serialized hashref.  The path prefix can be used for prepending all of the paths returned in the serialized hashref.

=== deserialize

   my $deserialized = $dss->deserialize($serialized);

Deserializes/expands a hash of path/data pairs.  Returns the expanded object, which is usually a hashref, but might be an arrayref.
For example:

   # Starts with an array
   my $serialized = {
      '[0].thingy' => 1,
      '[1].thingy' => 2,
   };
   my $deserialized = $dss->deserialize($serialized);

   # Returns:
   $deserialized = [
      { thingy => 1 },
      { thingy => 2 },
   ];

=== deserialize_pathval

   my $deserialized = $dss->deserialize_pathval($path, $value);

Deserializes/expands a single path/data pair.  Returns the expanded object.

=== merge

   my $newhash = $dss->merge($hash1, $hash2);

Merges two hashes.  This is a direct handle to {merge} from an (internal) [Hash::Merge] object, and is used by [/deserialize] to
combine individual expanded objects.

=== set_merge_behavior

Handle to {set_behavior} from the (internal) [Hash::Merge] object.  *Advanced usage only!*

Data::SplitSerializer uses a special custom type called {LEFT_PRECEDENT_STRICT_ARRAY_INDEX}, which properly handles array
indexes and dies on any non-array-or-hash refs.

=== specify_merge_behavior

Handle to {specify_behavior} from the (internal) [Hash::Merge] object.  *Advanced usage only!*

= CAVEATS

== Undefined values

Flattening will remove path/values if the value is undefined.  This is to clean up unused array values that appeared as holes in a
sparse array.  For example:

   # From one of the basic tests
   my $round_trip = $dss->serialize( $dss->deserialize_pathval(
      'a[0][1][1][1][1][2].too' => 'long'
   ) );

   # Without undef removal, this returns:
   $round_trip = {
      'a[0][0]'                 => undef,
      'a[0][1][0]'              => undef,
      'a[0][1][1][0]'           => undef,
      'a[0][1][1][1][0]'        => undef,
      'a[0][1][1][1][1][0]'     => undef,
      'a[0][1][1][1][1][1]'     => undef,
      'a[0][1][1][1][1][2].too' => 'long',
   };

You can disable this with the [/remove_undefs] switch.

== Refs in split serialization

Split serialization works by looking for HASH or ARRAY refs and diving further into them, adding path prefixes as it goes down.  If
it encounters some other ref (like a SCALAR), it will stop and consider that to be the value for that path.  In terms of ref parsing,
this means two things:

0 Only HASH and ARRAYs can be examined deeper.
0 If you have a HASH or ARRAY as a "value", serialization cannot tell the difference and it will be included in the path.

The former isn't that big of a problem, since deeper dives with other kinds of refs are either not possible or dangerous (like CODE).

The latter could be a problem if you started with a hashref with a path/data pair, expanded it, and tried to flatten it again.  This
can be solved by protecting the hash with a REF.  Consider this example:

   my $round_trip = $dss->serialize( $dss->deserialize_pathval(
      'a[0]' => { your => 'hash' }
   ) );

   # Returns:
   $round_trip = {
      'a[0].your' => 'hash',
   };

   # Now protect the hash
   my $round_trip = $dss->serialize( $dss->deserialize_pathval(
      'a[0]' => \{ your => 'hash' }
   ) );

   # Returns:
   $round_trip = {
      'a[0]' => \{ your => 'hash' }
   };

== Sparse arrays and memory usage

Since arrays within paths are based on indexes, there's a potential security issue with large indexes causing abnormal memory usage.
In Perl, these two arrays would have drastically different memory footprints:

   my @small;
   $small[0] = 1;

   my @large;
   $large[999999] = 1;

This can be mitigated by making sure the Path style you use will limit the total digits for array indexes.  [Parse::Path] handles
this on all of its paths, but it's something to be aware of if you create your own path classes.

= TODO

This module might split off into individual split serializers, but so far, this is the only one "out in the wild".

= SEE ALSO

[Parse::Path]

= ACKNOWLEDGEMENTS

Kent Fredric for getting me started on the basic idea.

=end wikidoc
