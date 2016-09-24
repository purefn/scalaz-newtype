package scalaz.newtype.deriving

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

// Derives type-class instances for newtypes. To usen annotate an object
// with one or more `@deriving` and the object will be filled with one or more
// `InstancesX` traits and an `Instances` trait which your newtypes companion
// object can extend. As an example consider,
//
// ```
// case class PosInt(value: Int)
//
// object PosInt extends PosIntInstances.Instance
//
// @deriving(Semigroup[PosInt])
// @deriving(Monoid[PosInt])
// object PosIntInstances
// ```
//
// The `PosIntInstances` trait will contain three traits:
//
//   1. `Instances0`, which will contain an implicit value for `Semigroup[PosInt]`
//   2. `Instances1`, which will contain an implicit value for `Monoid[PosInt]`
//      and extend `Instances0`.
//   3. `Instances`, which will not contain any values but extends `Instances1`.
//
// If a trait already exists in the object, as in
//
// ```
// @deriving(Monoid[PosInt])
// object PosIntInstances {
//   trait Inst extends OtherInstances
// }
// ```
//
// then no `Instances` trait will be created and `Inst` will be used. In addition,
// `Instances0` will extend `OtherInstances`.
//
// Type-classes that can be derived must follow a few rules:
//
// 1. The arity of the type-class instance being derived must take exactly one
//    type parameter.
//
// 2. The type-class type parameter can only occur in method parameter and return types:
//
//      a) directly, e.g. `A` in `trait TC[A] { def f(a: A, a: A): A }` or
//         `M` in `trait TC[M[_]] { def f[A, B](fa: M[A])(g: A => B): M[B]`
//
//      b) as a by-name type, e.g. `b: => A` in
//         `trait TC[A] { def f(a: A, b: => A): A }`
//
//      c) the return value of a function with arity of one, e.g. `g: A => M[B]` in
//         `trait TC[M[_]] { def f[A, B](fa: M[A])(g: A => M[B]): M[B] }`
//
//         TODO: can we generalize for any Functor?
//
// This implies at least one limitation on type-classes that can be derived.
//
// * Type-classes that have methods which take or return monad transformers cannot
//   be derived for a newtype, e.g. we can't derive an instance for
//
//   ```
//   trait Underivable[M[_]] {
//     def toStream(a: Int): StreamT[M, Int]
//     def fromStream(as: StreamT[M, Int]): Int
//   }
//
//   case class SomeNewtypeT[M[_], A](run: SomeOtherT[M, A])
//   ```
//
//   Attempting to derive `Underivable[SomeNewtypeT[M, ?]]` will result in an
//   errors because we don't attempt to convert from
//   `SomeNewtypeT[M, A]` to `SomeOtherT[M, A]` or vice-versa.
//
//   TODO: can we look for a `Hoist` instance for these types to support it in
//         at least some cases (maybe most cases)?
//
// There are probably others and they will be documented as they are found, if
// they can't be fixed.
//
// Derivation currently supports the following newtype cases:
//
// 1. Monomorphic newtypes, i.e. case classes without type parameters, e.g.
//
//    ```
//    case class X(value: Y)
//    ```
//
// 2. Simple polymorphic newtypes, i.e. case classes that take monomorphic type
//    parameters, e.g.
//
//    ```
//    case class SomeNewtype[A](ga: SomeTypeConstructor[A])
//    ```
//
// 3. Slightly complex polymorphic newtypes, where the instance needs type
//    parameters, as in the case of monad transformers
//
//    ```
//    case class SomeNewtypeT[M[_], A](run: SomeOtherT[M, A])
//    ```
//
//    However, this requires a some information to be added to the
//    macro, 1) the type parameter and 2) the context bounds required
//    to get an instance of the type-class for the field type.
//
//    To derive a `Functor` instance for `SomeNewtypeT`, we'd specify
//
//    ```
//    @deriving[M[_]](Functor[SomeNewtypeT[M, ?]])(Monad[M])
//    ```
//
//
@compileTimeOnly("enable macro paradise to expand macro annotations")
class deriving extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro derivingImpl.derive
}

sealed trait DerivationResult[A] {
  def fold[Z](success: A => Z, failure: String => Z): Z =
    this match {
      case DerivationResult.Success(a) => success(a)
      case DerivationResult.Failure(msg) => failure(msg)
    }

  def map[B](f: A => B): DerivationResult[B] =
    fold(a => DerivationResult.Success(f(a)), DerivationResult.Failure[B](_))

  def flatMap[B](f: A => DerivationResult[B]): DerivationResult[B] =
    fold(f(_), DerivationResult.Failure[B](_))

  def valueOr(f: String => A): A =
    fold(identity, f)
}

private[this] object DerivationResult {
  final case class Success[A](value: A) extends DerivationResult[A]
  final case class Failure[A](msg: String) extends DerivationResult[A]

  def success[A](a: A): DerivationResult[A] = Success(a)
  def failure[A](msg: String): DerivationResult[A] = Failure(msg)
}

final class derivingImpl(val c: Context) {
  import c.universe._
  import c.universe.internal._

  import DerivationResult._

  def derive(annottees: Expr[Any]*): Expr[Any] = {

    val q"new $_[..$tpt](...$tps).macroTransform(..$_)" = c.macroApplication

    // type arguments to the macro that are type constructors show
    // up as existential types, so we need to convert them into type parameters
    // TODO handle when it's not a type constructor
    // TODO handle other kinds, like T[_[_]]
    // TODO error handling
    val typeParams = tpt.map { t =>
      val tq"$n[..$ps] forSome { ..$defs }" = t

      val tmods = Modifiers(Flag.PARAM)
      val Ident(name) = n
      q"$tmods type ${name.toTypeName}[_]"
    }

    // because of the way we specify what we're deriving, the type-class name
    // shows up as a TermName and we need to convert it to a type name
    // TODO error handling - specifically,
    //      * if there are no paramerts, was simply `@deriving`
    //      * something that doesn't look like a type instance, `@deriving(Int)`
    val instanceTypeTree = {
      val TypeApply(Ident(n), ps) = tps.head.head
      tq"${n.toTypeName}[..$ps]"
    }

    val instanceType = {
      // we create a dummy class and use c.typecheck to get the type symbols
      // for the type-class and the newtype
      val n = TypeName(c.freshName("Dummy"))
      val e = q"""
        abstract class $n {
          def dummy[..$typeParams]: $instanceTypeTree
        }
        7.asInstanceOf[$n]
      """
      val m = c.typecheck(e).tpe.decls.filter(!_.isConstructor).head.asMethod
      m.returnType
    }

    // because of the way we specify what the bounds are, the type-class name
    // shows up as a TermName and we need to convert it to a type name
    // TODO can we look up context bounds in the companion objects of
    //      the type-class or value type?
    val contextBounds = {
      tps.tail.headOption.toList.flatten.map { t =>
        val TypeApply(Ident(n), ps) = t
        val tt = tq"${n.toTypeName}[..$ps]"

        q"val ${TermName(c.freshName("ev"))}: ${n.toTypeName}[..$ps]"
      }
    }

    val inputs = annottees.map(_.tree).toList
    val result =
      for {
        newtype <- Newtype(instanceType.typeArgs.head)
        typeClass = TypeClass(instanceType)
        instance = newtype.derive(typeParams, contextBounds, typeClass)
        tree <- reTree(inputs, instance)
      } yield c.Expr[Any](Block(List(c.untypecheck(tree)), Literal(Constant(()))))

// println(result)

    result.valueOr { msg =>
      c.error(c.enclosingPosition, msg)
      c.Expr[Any](Block(inputs, Literal(Constant(()))))
    }
  }

  def mkTypeDef(sym: Symbol): TypeDef = {
    val tsym = sym.asType
    q"${Modifiers(flags(sym))} type ${tsym.name.toTypeName}[..${tsym.typeParams.map(mkTypeDef(_))}]"
  }

  def mkTypTree(tpe: Type): Tree =
    tpe match {
      case PolyType(args, tpe1) =>
        tq"({type l[..${args.map(mkTypeDef(_))}] = ${mkTypTree(tpe1)}})#l"
      case TypeRef(NoPrefix, sym, args) =>
        tq"${TypeName(sym.name.decodedName.toString)}[..${args.map(mkTypTree)}]"
      case TypeRef(pre, sym, args) =>
        tq"${pre}#${TypeName(sym.name.decodedName.toString)}[..${args.map(mkTypTree)}]"
    }

  private[this] def reTree(inputs: List[Tree], instance: Tree): DerivationResult[Tree] = {
    def newTrait(name: TypeName, parent: Option[Tree], i: Int) = {
      val n: TypeName = TypeName(s"${name.decodedName}$i")
      val t =
        parent.map(p => q"sealed trait $n extends $p { $instance }").
          getOrElse(q"sealed trait $n { $instance }")

      (n, t)
    }

    // disassemble the annotated object.
    // put trait(s) for the type-class we're deriving in it, extending existing traits.
    //
    // ASSUMPTION: the highest priority type-class instances will be in the first trait
    // one defined in the object and it will extend the second one defined, which will
    // extend the third, and so on.
    inputs match {
      case List(q"$omods object $oname extends { ..$oearlydefns } with ..$oparents { $oself => ..$obody }") =>
        val nbody =
          obody.headOption.
            map(_ => obody).
            getOrElse(List(q"trait Instances"))
            match {
              case q"$mods trait $n extends $parent" :: xs =>
                val (ntname, nt) = newTrait(n, Some(parent), obody.length)
                q"$mods trait $n extends $ntname" :: nt :: xs
              case q"$mods trait $n" :: xs =>
                val (ntname, nt) = newTrait(n, None, obody.length)
                q"$mods trait $n extends $ntname" :: nt :: xs
            }

        success(q"$omods object $oname extends { ..$oearlydefns } with ..$oparents { $oself => ..$nbody }")
      case _ =>
        failure("deriving macro can only be applied to objects")
    }
  }

  sealed class TypeClass
    ( val tpe: Type
    , val name: TypeName
    , val typeParams: List[Symbol]
    , val abstractMethods: List[MethodSymbol]
    ) {

    override def toString: String =
      s"TypeClass(name = $name, typeParams = $typeParams, abstractMethods = $abstractMethods)"
  }

  object TypeClass {
    def apply(tpe: Type): TypeClass = {
      val classSymbol: ClassSymbol = tpe.typeSymbol.asClass
      val abstractMethods: List[MethodSymbol] =
        tpe.members.
          filter(_.isMethod).
          map(_.asMethod).
          filter(_.isAbstract).
          toList

      new TypeClass(tpe, classSymbol.name, classSymbol.typeParams, abstractMethods)
    }
  }

  final class Newtype(tpe: Type, field: Field) {
    val symbol = tpe.typeSymbol.asClass
    val name = symbol.name
    val typeParams = symbol.typeParams

    def derive(typeParams: List[TypeDef], contextBounds: List[ValDef], typeClass: TypeClass): Tree = {
      val fieldInstance = field.instanceDef(typeClass)
      val defs = typeClass.abstractMethods.map(deriveDef(typeClass, fieldInstance, _))

      val iname = TermName(name.decodedName.toString + typeClass.name.decodedName.toString)
      val itype = mkTypTree(typeClass.tpe)

      if (typeParams.isEmpty)
        q"""
          implicit val $iname: $itype = new $itype {
            ${fieldInstance}
            ..$defs
          }
        """
      else
        q"""
          implicit def $iname[..$typeParams](implicit ..$contextBounds): $itype =
            new $itype {
              $fieldInstance
              ..$defs
            }
        """
    }

    def deriveDef(typeClass: TypeClass, fieldInstance: ValDef, m: MethodSymbol) = {
      val mname = m.name
      val info = m.infoIn(typeClass.tpe)
      val q"$_ def $_[..$tparams](...$_): $_ = $_" = defDef(m, q"???")
      val params = info.paramLists.map(_.map(massageDefParam))

      q"""
        override def $mname[..$tparams](...${params.map(_.map(_._1))}) =
          ${name.toTermName}(${fieldInstance.name}.$mname(...${params.map(_.map(_._2))}))
      """
    }

    def massageDefParam(param: Symbol) = {
      val p = valDef(param)
      val q"$pmods val $pname: $ptpt = $pbody" = p
      val ptype = param.typeSignature
      val select =
        if (ptype =:= tpe)
          Some(q"$pname.${field.name}")
        // special case by-name parameters
        else if (param.asTerm.isByNameParam && ptype.typeArgs.head.typeConstructor =:= tpe.typeConstructor)
          Some(q"$pname.${field.name}")
        // special case for when the parameter is a function and the
        // result is a value of the newtype, e.g. A => ThingT[M, B].
        // TODO can this be generalized, maybe by looking for a
        //      Functor instance for ptype?
        else if (ptype.typeSymbol.name == TypeName("Function1")) {
          ptype.typeArgs.lastOption.
            filter(!_.typeArgs.isEmpty).
            map(t => polyType(t.typeArgs.lastOption.map(_.typeSymbol).toList, t)).
            filter(_.erasure =:= tpe.erasure).
            map(_ => q"$pname.andThen(_.${field.name})")
        } else {
          ptype.typeArgs.lastOption.
            map(a => polyType(List(a.typeSymbol), ptype)).
            filter(_.erasure =:= tpe.erasure).
            map(_ => q"$pname.${field.name}")
        }
      (q"$pmods val $pname: ${mkTypTree(param.typeSignature)} = $pbody", select.getOrElse(q"$pname"))
    }

    override def toString: String =
      s"Newtype(name = $name, typeParams = $typeParams, field = $field)"
  }

  object Newtype {
    def apply(tpe: Type): DerivationResult[Newtype] = {
      val symbol: ClassSymbol = tpe.typeSymbol.asClass
      val name = symbol.name

      val fs = Field.forType(tpe)

      if (!symbol.isCaseClass)
        failure(s"deriving macro can only derive instances for case classes with a single value, $name is not a case class")
      else if (fs.length != 1)
        failure(s"deriving macro can only derive instances for case classes with a single value, $name has ${fs.length}")
      else success(new Newtype(tpe, fs.head))
    }
  }

  sealed class Field
    ( val tpe: Type
    , val name: TermName
    , val typeName: TypeName
    , val typeParams: List[Type]
    ) {

    // what we'd like to do is lookup the implicit type-class instance for the field type,
    // then use it to determine the context bounds of the derived instance.
    // e.g. if we're deriving a `Functor` instance and the field
    // has a type of `Kleisli[M, R, A]`, we need to lookup the `Functor`
    // instance for `Kleisli[M, R, ?]`, determine what context bounds are needed
    // to get that instance, i.e. `Functor[M]`, and use those as the context bounds
    // for the derived `Functor` instance.
    //
    // but i haven't been able to figure out how to do that, so instead we're requiring
    // users to manually provide the context bounds in such cases.
    // this is less than ideal , but until a better solution presents itself,
    // this will have to do.
    // TODO find a better solution to ^
    //      as a first step it should be possible to check the companion objects of
    //      the value type and the type-class. this should work in 90% of the cases
    //      and should only miss orphan instances.

    // def contextBounds(typeClass: TypeClass): List[Tree] = {
    //   val instanceType =
    //     if (typeParams.isEmpty)
    //       tq"${typeClass.name.toTypeName}[$tpe]"
    //     else
    //       tq"${typeClass.name.toTypeName}[$typeLambda]"

    //   List(q"val ${TermName(c.freshName("ev"))}: $instanceType")
    // }

    def instanceDef(typeClass: TypeClass): ValDef = {
      val n = TermName(c.freshName(typeName.decodedName.toString + typeClass.name.decodedName.toString))

      if (tpe.typeArgs.isEmpty || tpe.typeArgs.forall(!_.typeSymbol.isParameter))
        q"""val $n = implicitly[${typeClass.name}[${mkTypTree(tpe)}]]"""
      else {
        val ptpe = polyType(tpe.typeArgs.reverse.headOption.toList.map(_.typeSymbol), tpe)

        // we create a type alias to help scala with inference in some cases,
        // e.g monad transformer stacks. this stinks because it means implicit
        // resolution errors look like
        //
        //   could not find implicit value for parameter e: scalaz.Functor[Kleisli$macro$3]
        //
        // instead of the much more helpful
        //
        //    could not find implicit value for parameter e: scalaz.Functor[Kleisli[M, X, ?]]
        //
        // but that's what we're left with for now
        val nalias = TypeName(c.freshName(tpe.typeConstructor.typeSymbol.name.decodedName.toString))
        val naliasParam = TypeName(c.freshName("A"))
        val talias = {
          val (select, args) =
            tpe match {
              case TypeRef(NoPrefix, sym, args) =>
                (tq"${TypeName(sym.name.decodedName.toString)}", args)
              case TypeRef(pre, sym, args) =>
                (tq"${pre}#${TypeName(sym.name.decodedName.toString)}", args)
            }
          val nargs = (tq"$naliasParam" :: args.reverse.drop(1).map(mkTypTree)).reverse
          tq"$select[..$nargs]"
        }
        q"""
          val $n: ${typeClass.name}[${mkTypTree(ptpe)}] = {
            type $nalias[$naliasParam] = $talias
            implicitly[${typeClass.name}[$nalias]]
          }
        """
      }
    }

    override def toString: String =
      s"Field(name = $name, typeName = $typeName, typeParams = $typeParams)"
  }

  object Field {
    def apply(symbol: Symbol, ctpe: Type): Field = {
      val tpe = symbol.typeSignature
      val name = TermName(symbol.asTerm.name.decodedName.toString)
      new Field(tpe, name, tpe.typeSymbol.asClass.name, tpe.typeArgs)
    }

    def forType(tpe: Type): List[Field] =
      tpe.decls.
        filter(_.isMethod).
        map(_.asMethod).
        filter(_.isPrimaryConstructor).
        flatMap(_.paramLists.head).
        map(Field(_, tpe)).
        toList
  }
}

