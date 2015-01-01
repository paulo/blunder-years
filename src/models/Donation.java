/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

import java.util.GregorianCalendar;

/**
 *
 * @author tiago
 */
public class Donation {
    private Integer id;
    private Integer donationType;
    private GregorianCalendar donationDate;
    private Integer quantity;
    private Double amount;
    private Boolean used;
    private String observations;

    public Donation() {}
    
    public Donation(Integer donationType, GregorianCalendar dontationDate, Integer quantity, Double amount, Boolean used, String observations) {
        this.id = -1;
        this.donationType = donationType;
        this.donationDate = dontationDate;
        this.quantity = quantity;
        this.amount = amount;
        this.used=used;
        this.observations = observations;
    }
    
    public Donation(Donation don){
        this.id = don.getId();
        this.donationType = don.getDonationType();
        this.donationDate = don.getDonationDate();
        this.quantity = don.getQuantity();
        this.amount = don.getAmount();
        this.used=don.getUsed();
        this.observations = don.getObservations();
    }

    public Integer getId() {
        return id;
    }
    
    public void setId(Integer id) {
        this.id = id;
    }


    public Integer getDonationType() {
        return donationType;
    }

    public GregorianCalendar getDonationDate() {
        return donationDate;
    }

    public Integer getQuantity() {
        return quantity;
    }

    public Double getAmount() {
        return amount;
    }
    
    public Boolean getUsed() {
        return used;
    }

    public String getObservations() {
        return observations;
    }

    public void setDonationType(Integer donationType) {
        this.donationType = donationType;
    }

    public void setDonationDate(GregorianCalendar dontationDate) {
        this.donationDate = dontationDate;
    }

    public void setQuantity(Integer quantity) {
        this.quantity = quantity;
    }

    public void setAmount(Double amount) {
        this.amount = amount;
    }
    
    public void setUsed(Boolean used) {
        this.used=used;
    }

    public void setObservations(String observations) {
        this.observations = observations;
    }
    
    @Override
    public Donation clone(){
        return new Donation(this);
    }
    
    public int hashCode()  {
        return id;
    }
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\n");
        sb.append(id);
        sb.append(", ");
        sb.append(donationType);
        sb.append(", ");
        sb.append(donationDate);
        sb.append(", ");
        sb.append(amount);
        sb.append(", ");
        sb.append(quantity);
        return sb.toString();
    }
    
    @Override
    public boolean equals (Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Donation don = (Donation) o;
        
        return ( this.donationType == don.getDonationType() && this.donationDate.equals(don.getDonationDate()) && this.amount == don.getAmount() && this.quantity == don.getQuantity() );
    }
}
