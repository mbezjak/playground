package interop;

public class TraitImplUse {

    public static void main(String[] args) {
        MyTrait quux = TraitImpl$.MODULE$.apply("quux");
        System.out.println(quux.traitName());

        MyTrait foo = TraitImpl.apply();
        System.out.println(foo.traitName());

        MyTrait bar = TraitImpl.apply("bar");
        System.out.println(bar.traitName());
    }

}
