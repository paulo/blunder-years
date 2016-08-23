/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

/**
 *
 * @author joaorodrigues
 */
public class PaymentPlan extends BasicModel {
    private Float nextPayment;
    private String notes;

    public PaymentPlan(){
        super();
    }
    
    public PaymentPlan(Float nextPayment, String notes ) {
        super(-1);
        this.nextPayment = nextPayment;
        this.notes = notes;
    }

    public Float getNextPayment() {
        return nextPayment;
    }

    public String getNotes() {
        return notes;
    }

    public void setnextPayment(Float nextPayment) {
        this.nextPayment = nextPayment;
    }

    public void setNotes(String notes) {
        this.notes = notes;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        PaymentPlan p = (PaymentPlan) obj;
        
        return ( super.equals(obj) );
    }
    
}
