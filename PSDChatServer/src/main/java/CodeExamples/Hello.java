package CodeExamples;

import co.paralleluniverse.actors.*;
import co.paralleluniverse.fibers.SuspendExecution;

public class Hello {

    static class Request {

        //ActorRef é uma referencia para um actor, tipo uma camada de abstraçao extra
        final ActorRef<String> from;
        final String str;

        Request(ActorRef<String> from, String str) {
            this.from = from;
            this.str = str;
        }
    }

    public static void main(String args[]) throws Exception {
        final String[] strings = {"Hello", "World"};
        //o método spawn retornar um actorref. Depois de fazer spawn, o método doRun é invocado
        final ActorRef<Request> server = new ToUpperActor().spawn();
        //criado um novo basicactor(permite selective receiveing) que recebe mensagens do tipo string e o doRun retorna void
        Actor<String, Void> client = new BasicActor<String, Void>() {
            @Override
            protected Void doRun() throws InterruptedException, SuspendExecution {
                //obter a actorRef para este actor, para enviar para o Toupperactor e este conseguir responder
                ActorRef<String> self = this.ref();
                for (String s : strings) {
                    //enviar as strings uma por uma, e esperar a resposta do actor. Receive vai à mailbox buscar uma mensagem, e
                    //se nao estiver la nenhuma, bloqueia até receber
                    Request req = new Request(self, s);
                    server.send(req);
                    String reply = receive();
                    System.out.println(reply);
                }
                return null;
            }
        };
        client.spawn();
        client.join();
    }

    //Actor usado para converter as strings. Receve do actor client e retorna por mensagem a string em uppercase
    static class ToUpperActor extends BasicActor<Request, Void> {

        @Override
        protected Void doRun() throws InterruptedException, SuspendExecution {
            for (;;) {
                Request m = receive();
                String reply = m.str.toUpperCase();
                m.from.send(reply);
            }
        }
    }
}
